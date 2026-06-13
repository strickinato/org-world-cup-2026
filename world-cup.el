;;; world-cup.el --- Browse FIFA World Cup 2026 data -*- lexical-binding: t; -*-

;; Author: org-world-cup-2026
;; Keywords: games, convenience
;; Package-Requires: ((emacs "27.1") (magit-section "3.0"))

;;; Commentary:

;; Tools for browsing the FIFA World Cup 2026 data that lives in
;; `data/world-cup-2026-rosters.json' and `data/world-cup-2026-schedule.json'.
;;
;; Main entry point:
;;
;;   M-x world-cup-consult-team
;;     Pick a team (with `consult' if available, otherwise plain
;;     `completing-read') and pop up a buffer in `world-cup-team-mode'
;;     showing two collapsible sections (via `magit-section'):
;;       * Fixtures - the team's group-stage matches
;;       * Squad    - the full 26-player roster
;;
;; The two JSON files are joined on the FIFA 3-letter team code.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'url)
(require 'transient)
(require 'magit-section)
(require 'hbut)  ; GNU Hyperbole explicit buttons (ebut:program)

(defgroup world-cup nil
  "Browse FIFA World Cup 2026 data."
  :group 'games
  :prefix "world-cup-")

(defcustom world-cup-data-directory
  (let ((here (or load-file-name buffer-file-name default-directory)))
    (expand-file-name "data" (file-name-directory here)))
  "Directory containing the World Cup JSON data files."
  :type 'directory
  :group 'world-cup)

(defcustom world-cup-rosters-file "world-cup-2026-rosters.json"
  "Name of the rosters JSON file inside `world-cup-data-directory'."
  :type 'string
  :group 'world-cup)

(defcustom world-cup-schedule-file "world-cup-2026-schedule.json"
  "Name of the schedule JSON file inside `world-cup-data-directory'."
  :type 'string
  :group 'world-cup)

(defcustom world-cup-analysis-file "world-cup-2026-team-analysis.json"
  "Name of the team analysis JSON inside `world-cup-data-directory'."
  :type 'string
  :group 'world-cup)

(defcustom world-cup-fixture-notes-file "world-cup-2026-fixture-notes.json"
  "Name of the per-fixture notes JSON inside `world-cup-data-directory'."
  :type 'string
  :group 'world-cup)

(defcustom world-cup-fox-rankings-file "world-cup-2026-fox-rankings.json"
  "Name of the FOX Sports top-100 rankings JSON inside `world-cup-data-directory'."
  :type 'string
  :group 'world-cup)

(defcustom world-cup-power-rankings-file "world-cup-2026-power-rankings.json"
  "Name of the team power-rankings JSON inside `world-cup-data-directory'."
  :type 'string
  :group 'world-cup)

(defcustom world-cup-history-file "world-cup-2026-history.json"
  "Name of the team World Cup history JSON inside `world-cup-data-directory'."
  :type 'string
  :group 'world-cup)

(defcustom world-cup-results-file "world-cup-2026-results.json"
  "Name of the live results JSON inside `world-cup-data-directory'.
Written by `world-cup-refresh-results' from ESPN's public endpoints."
  :type 'string
  :group 'world-cup)

;;;; Faces

(defgroup world-cup-faces nil
  "Faces used in World Cup buffers." :group 'world-cup)

(defface world-cup-title
  '((t :inherit info-title-2 :weight bold))
  "Face for the main title of a World Cup buffer." :group 'world-cup-faces)

(defface world-cup-subtitle
  '((t :inherit shadow :slant italic))
  "Face for a subtitle line beneath a title." :group 'world-cup-faces)

(defface world-cup-heading
  '((t :inherit (outline-1 magit-section-heading) :weight bold :height 1.2 :overline t))
  "Face for section headings (Analysis, Power Rankings, History, Fixtures, Squad)."
  :group 'world-cup-faces)

(defface world-cup-label
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for field labels such as \"Team:\"." :group 'world-cup-faces)

(defface world-cup-meta
  '((t :inherit shadow))
  "Face for dim secondary text (hints, venues, footers)." :group 'world-cup-faces)

(defface world-cup-column-header
  '((t :inherit (bold shadow) :underline t))
  "Face for table column headers." :group 'world-cup-faces)

(defface world-cup-code
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for 3-letter team codes." :group 'world-cup-faces)

(defface world-cup-quote
  '((t :inherit font-lock-doc-face :slant italic))
  "Face for editorial summaries and quotes." :group 'world-cup-faces)

(defface world-cup-note
  '((t :inherit font-lock-string-face :slant italic))
  "Face for fixture notes." :group 'world-cup-faces)

(defface world-cup-rank
  '((t :inherit warning :weight bold))
  "Face for FOX Sports ranking markers." :group 'world-cup-faces)

(defface world-cup-player-link '((t :inherit link))
  "Face for a clickable player name (Hyperbole implicit button)."
  :group 'world-cup-faces)

(defface world-cup-position-gk '((t :inherit font-lock-builtin-face :weight bold))
  "Face for goalkeepers." :group 'world-cup-faces)
(defface world-cup-position-df '((t :inherit font-lock-keyword-face :weight bold))
  "Face for defenders." :group 'world-cup-faces)
(defface world-cup-position-mf '((t :inherit font-lock-function-name-face :weight bold))
  "Face for midfielders." :group 'world-cup-faces)
(defface world-cup-position-fw '((t :inherit font-lock-string-face :weight bold))
  "Face for forwards." :group 'world-cup-faces)

;; Backward-compatible alias for the former title face name.
(put 'world-cup-summary-title 'face-alias 'world-cup-title)
(defface world-cup-summary-title '((t :inherit world-cup-title)) "Alias."
  :group 'world-cup-faces)


;;;; Data loading

(defvar world-cup--teams nil "Cached list of team alists.")
(defvar world-cup--matches nil "Cached list of match alists.")
(defvar world-cup--analysis nil "Cached alist of CODE -> analysis alist.")
(defvar world-cup--fixture-notes nil "Cached alist of match-number -> note string.")
(defvar world-cup--fox-rankings nil "Cached alist of CODE -> (NUMBER -> ranking alist).")
(defvar world-cup--power-rankings nil "Cached alist of CODE -> list of power-ranking alists.")
(defvar world-cup--history nil "Cached alist of CODE -> history alist.")
(defvar world-cup--results nil "Cached results alist (matches + scorers + updated).")

(defun world-cup--path (file)
  "Return the absolute path of FILE inside `world-cup-data-directory'."
  (expand-file-name file world-cup-data-directory))

(defun world-cup--read-json (path)
  "Parse JSON file at PATH using alists and lists."
  (unless (file-readable-p path)
    (error "World Cup data file not found: %s" path))
  (if (fboundp 'json-parse-string)
      (with-temp-buffer
        (insert-file-contents path)
        (json-parse-buffer :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object nil))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (json-read-file path))))

(defun world-cup-load-data (&optional force)
  "Load and cache teams and matches, re-reading from disk if FORCE."
  (when (or force (null world-cup--teams))
    (setq world-cup--teams
          (alist-get 'teams (world-cup--read-json
                             (world-cup--path world-cup-rosters-file)))))
  (when (or force (null world-cup--matches))
    (setq world-cup--matches
          (alist-get 'matches (world-cup--read-json
                              (world-cup--path world-cup-schedule-file)))))
  (when (or force (null world-cup--analysis))
    (let ((path (world-cup--path world-cup-analysis-file)))
      (setq world-cup--analysis
            (when (file-readable-p path)
              (alist-get 'analysis (world-cup--read-json path))))))
  (when (or force (null world-cup--fixture-notes))
    (let ((path (world-cup--path world-cup-fixture-notes-file)))
      (setq world-cup--fixture-notes
            (when (file-readable-p path)
              (alist-get 'notes (world-cup--read-json path))))))
  (when (or force (null world-cup--fox-rankings))
    (let ((path (world-cup--path world-cup-fox-rankings-file)))
      (setq world-cup--fox-rankings
            (when (file-readable-p path)
              (alist-get 'rankings (world-cup--read-json path))))))
  (when (or force (null world-cup--power-rankings))
    (let ((path (world-cup--path world-cup-power-rankings-file)))
      (setq world-cup--power-rankings
            (when (file-readable-p path)
              (alist-get 'rankings (world-cup--read-json path))))))
  (when (or force (null world-cup--history))
    (let ((path (world-cup--path world-cup-history-file)))
      (setq world-cup--history
            (when (file-readable-p path)
              (alist-get 'history (world-cup--read-json path))))))
  (when (or force (null world-cup--results))
    (let ((path (world-cup--path world-cup-results-file)))
      (setq world-cup--results
            (when (file-readable-p path)
              (world-cup--read-json path)))))
  (cons world-cup--teams world-cup--matches))

;;;###autoload
(defun world-cup-reload-data ()
  "Force a reload of the World Cup data from disk."
  (interactive)
  (world-cup-load-data t)
  (message "World Cup: loaded %d teams, %d matches"
           (length world-cup--teams) (length world-cup--matches)))

(defun world-cup-teams () (car (world-cup-load-data)))
(defun world-cup-matches () (cdr (world-cup-load-data)))

;;;; Accessors

(defun world-cup-team-name (team) (alist-get 'team team))
(defun world-cup-team-code (team) (alist-get 'code team))
(defun world-cup-team-players (team) (alist-get 'players team))

(defun world-cup-team-coach-name (team)
  (let ((coach (alist-get 'coach team)))
    (and coach (alist-get 'name coach))))

(defun world-cup-team-analysis (team)
  "Return the analysis alist for TEAM, or nil."
  (world-cup-load-data)
  (when-let ((code (world-cup-team-code team)))
    (alist-get (intern code) world-cup--analysis)))

(defun world-cup-match-note (match)
  "Return the fixture note string for MATCH, or nil."
  (world-cup-load-data)
  (when-let ((n (alist-get 'match_number match)))
    (alist-get (intern (number-to-string n)) world-cup--fixture-notes)))

(defun world-cup-fox-ranking (code number)
  "Return the FOX ranking alist (rank, summary, name) for CODE + NUMBER, or nil."
  (world-cup-load-data)
  (when (and code number)
    (alist-get (intern (number-to-string number))
               (alist-get (intern code) world-cup--fox-rankings))))

(defun world-cup-player-fox-ranking (team player)
  "Return the FOX ranking alist for PLAYER of TEAM, or nil."
  (world-cup-fox-ranking (world-cup-team-code team)
                         (alist-get 'number player)))

(defun world-cup-team-power-rankings (team)
  "Return the list of power-ranking alists (source, rank, description) for TEAM."
  (world-cup-load-data)
  (when-let ((code (world-cup-team-code team)))
    (alist-get (intern code) world-cup--power-rankings)))

(defun world-cup-team-history (team)
  "Return the World Cup history alist for TEAM, or nil."
  (world-cup-load-data)
  (when-let ((code (world-cup-team-code team)))
    (alist-get (intern code) world-cup--history)))

(defun world-cup-match-result (match)
  "Return the live-result alist for MATCH (keyed by match number), or nil."
  (world-cup-load-data)
  (when-let ((n (alist-get 'match_number match)))
    (alist-get (intern (number-to-string n))
               (alist-get 'matches world-cup--results))))

(defun world-cup-scorers ()
  "Return the golden-boot list (alist entries with player, code, goals)."
  (world-cup-load-data)
  (alist-get 'scorers world-cup--results))

(defun world-cup-player-stats (team player)
  "Return PLAYER's aggregated tournament stats alist for TEAM, or nil."
  (world-cup-load-data)
  (when-let ((code (world-cup-team-code team))
             (num (alist-get 'number player)))
    (alist-get (intern (format "%s-%d" code num))
               (alist-get 'players world-cup--results))))

(defun world-cup-assist-leaders ()
  "Return players with at least one assist, sorted by assists descending."
  (world-cup-load-data)
  (let (out)
    (dolist (kv (alist-get 'players world-cup--results))
      (let ((v (cdr kv)))
        (when (> (or (alist-get 'assists v) 0) 0) (push v out))))
    (sort out (lambda (x y) (> (alist-get 'assists x) (alist-get 'assists y))))))

(defun world-cup--find-team-by-code (code)
  (seq-find (lambda (team) (equal (world-cup-team-code team) code))
            (world-cup-teams)))

(defun world-cup--find-team-by-name (name)
  (seq-find (lambda (team) (equal (world-cup-team-name team) name))
            (world-cup-teams)))

(defun world-cup-team-matches (team)
  "Return the matches involving TEAM, joined on the 3-letter code.
Sorted by match number."
  (let ((code (world-cup-team-code team)))
    (seq-sort-by
     (lambda (m) (alist-get 'match_number m))
     #'<
     (seq-filter
      (lambda (m)
        (or (equal (alist-get 'team_a_code m) code)
            (equal (alist-get 'team_b_code m) code)))
      (world-cup-matches)))))

;;;; Match times (convert the schedule's Eastern time to a display zone)

(defcustom world-cup-display-time-zone "America/Los_Angeles"
  "Time zone used to display match times.
The schedule stores kickoff times in US Eastern time; they are converted to
this zone for display.  The value is passed to `format-time-string' as ZONE:
  - a zone name string such as \"America/Los_Angeles\" (Pacific, the default);
  - t for the system local time zone;
  - nil for UTC."
  :type '(choice (string :tag "Zone name")
                 (const :tag "System local" t)
                 (const :tag "UTC" nil))
  :group 'world-cup)

(defconst world-cup--source-time-zone "America/New_York"
  "Time zone in which the schedule's `time_et' values are expressed.")

(defun world-cup--match-time (match)
  "Return the absolute Lisp time of MATCH's kickoff, or nil.
The stored date and `time_et' are interpreted in `world-cup--source-time-zone'."
  (let ((date (alist-get 'date match))
        (et (alist-get 'time_et match)))
    (when (and (stringp date) (stringp et)
               (string-match
                "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" date))
      (let ((y (string-to-number (match-string 1 date)))
            (mo (string-to-number (match-string 2 date)))
            (d (string-to-number (match-string 3 date))))
        (when (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'" et)
          (let ((h (string-to-number (match-string 1 et)))
                (mi (string-to-number (match-string 2 et))))
            (encode-time
             (list 0 mi h d mo y nil -1 world-cup--source-time-zone))))))))

(defun world-cup--local-time (match)
  "Return MATCH's kickoff time HH:MM in `world-cup-display-time-zone'."
  (let ((time (world-cup--match-time match)))
    (if time
        (format-time-string "%H:%M" time world-cup-display-time-zone)
      (or (alist-get 'time_et match) "?"))))

(defun world-cup--local-date (match)
  "Return MATCH's date YYYY-MM-DD in `world-cup-display-time-zone'."
  (let ((time (world-cup--match-time match)))
    (if time
        (format-time-string "%Y-%m-%d" time world-cup-display-time-zone)
      (or (alist-get 'date match) "?"))))

(defun world-cup--tz-abbrev (&optional match)
  "Return the display zone abbreviation (e.g. PDT) for MATCH, or for now."
  (format-time-string "%Z"
                      (or (and match (world-cup--match-time match)) (current-time))
                      world-cup-display-time-zone))

;;;; Position faces / sorting

(defun world-cup--pos-face (pos)
  (pcase pos
    ("GK" 'world-cup-position-gk)
    ("DF" 'world-cup-position-df)
    ("MF" 'world-cup-position-mf)
    ("FW" 'world-cup-position-fw)
    (_ 'default)))

;;;; Team selection (consult / completing-read)

(defun world-cup--team-candidates ()
  "Return an alist of (DISPLAY . TEAM) for completion."
  (mapcar (lambda (team) (cons (world-cup-team-name team) team))
          (world-cup-teams)))

(defun world-cup--annotate (cands)
  "Return an annotation function for team candidate alist CANDS."
  (lambda (cand)
    (when-let ((team (cdr (assoc cand cands))))
      (concat
       (propertize (format "  [%s]" (world-cup-team-code team))
                   'face 'world-cup-label)
       (propertize (format "  %d players" (length (world-cup-team-players team)))
                   'face 'world-cup-meta)
       (when-let ((coach (world-cup-team-coach-name team)))
         (propertize (format "  coach: %s" coach)
                     'face 'world-cup-meta))))))

(defun world-cup--read-team ()
  "Prompt for a team, returning the chosen team alist.
Uses `consult--read' when available; otherwise `completing-read'."
  (let* ((cands (world-cup--team-candidates))
         (annotate (world-cup--annotate cands))
         (names (mapcar #'car cands))
         (choice
          (if (fboundp 'consult--read)
              (consult--read names
                             :prompt "World Cup team: "
                             :category 'world-cup-team
                             :require-match t
                             :sort t
                             :annotate annotate)
            (let ((completion-extra-properties
                   (list :annotation-function annotate)))
              (completing-read "World Cup team: " names nil t)))))
    (cdr (assoc choice cands))))

;;;; Rendering helpers

(defun world-cup--pad (s width)
  "Truncate or pad string S to WIDTH columns."
  (let ((s (or s "")))
    (truncate-string-to-width s width 0 ?\s)))

(defun world-cup--heading (text)
  "Insert TEXT as a `magit' section heading styled with `world-cup-heading'.
`magit-insert-heading' doesn't preserve a face passed in the string, so the
face is applied to the heading region afterwards."
  (let ((beg (point)))
    (magit-insert-heading text)
    (add-face-text-property
     beg (save-excursion (goto-char beg) (line-end-position))
     'world-cup-heading)))

;;;; Wikipedia lookup (consult + EWW reader mode)

(defcustom world-cup-wikipedia-language "en"
  "Wikipedia language subdomain used for lookups."
  :type 'string
  :group 'world-cup)

(defun world-cup--wikipedia-search (query)
  "Query Wikipedia's opensearch API for QUERY.
Return a list of plists with :title, :desc and :url."
  (let* ((endpoint (format "https://%s.wikipedia.org/w/api.php" world-cup-wikipedia-language))
         (url (format "%s?action=opensearch&format=json&namespace=0&limit=15&search=%s"
                      endpoint (url-hexify-string query)))
         (buf (url-retrieve-synchronously url t t 10)))
    (unless buf (error "Wikipedia request failed for %S" query))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (unless (re-search-forward "\n\n" nil t)
            (error "Malformed Wikipedia response"))
          (let* ((data (json-parse-buffer :array-type 'list :object-type 'alist
                                          :null-object nil :false-object nil))
                 (titles (nth 1 data))
                 (descs  (nth 2 data))
                 (urls   (nth 3 data)))
            (seq-mapn (lambda (tt d u) (list :title tt :desc d :url u))
                      titles descs urls)))
      (kill-buffer buf))))

(defcustom world-cup-user-agent
  "org-world-cup-2026/1.0 (https://github.com/; Emacs)"
  "User-Agent string sent with Wikipedia API requests.
The Wikimedia REST API asks clients to identify themselves."
  :type 'string
  :group 'world-cup)

(defcustom world-cup-summary-image-max-width 240
  "Maximum width (px) for the thumbnail in a summary overlay."
  :type 'integer :group 'world-cup)

(defcustom world-cup-summary-fill-column 74
  "Column at which to wrap the summary text."
  :type 'integer :group 'world-cup)

(defun world-cup--http-get (url &optional binary)
  "GET URL synchronously, returning the response body string.
With BINARY non-nil, return raw bytes (unibyte). Returns nil on failure."
  (let ((url-request-extra-headers
         (list (cons "User-Agent" world-cup-user-agent))))
    (condition-case nil
        (let ((buf (url-retrieve-synchronously url t t 12)))
          (when buf
            (unwind-protect
                (with-current-buffer buf
                  (when binary (set-buffer-multibyte nil))
                  (goto-char (point-min))
                  (when (re-search-forward "\r?\n\r?\n" nil t)
                    (buffer-substring-no-properties (point) (point-max))))
              (kill-buffer buf))))
      (error nil))))

(defun world-cup--wikipedia-summary (title)
  "Fetch the Wikipedia REST page summary for TITLE as an alist, or nil."
  (let* ((endpoint (format "https://%s.wikipedia.org/api/rest_v1/page/summary/%s"
                          world-cup-wikipedia-language
                          (url-hexify-string title)))
         (body (world-cup--http-get endpoint)))
    (when body
      (condition-case nil
          (json-parse-string body :object-type 'alist :array-type 'list
                             :null-object nil :false-object nil)
        (error nil)))))

(defun world-cup--fetch-image (url &rest props)
  "Fetch image at URL and return an image object scaled per PROPS, or nil."
  (when (and url (display-graphic-p))
    (let ((data (world-cup--http-get url t)))
      (when data
        (condition-case nil
            (apply #'create-image data nil t props)
          (error nil))))))

(defun world-cup--wikipedia-extract (title)
  "Fetch the full plain-text article extract for TITLE via the Action API.
Section headings are kept in wiki form (== Heading ==).  Returns nil on error."
  (let* ((endpoint (format "https://%s.wikipedia.org/w/api.php" world-cup-wikipedia-language))
         (url (format (concat "%s?action=query&format=json&prop=extracts"
                              "&explaintext=1&exsectionformat=wiki&redirects=1&titles=%s")
                      endpoint (url-hexify-string title)))
         (body (world-cup--http-get url)))
    (when body
      (condition-case nil
          (let* ((data (json-parse-string body :object-type 'alist :array-type 'list
                                          :null-object nil :false-object nil))
                 (pages (alist-get 'pages (alist-get 'query data)))
                 (page (cdar pages)))
            (alist-get 'extract page))
        (error nil)))))

(defun world-cup--insert-article-body (text)
  "Insert TEXT as a filled article body, fontifying wiki section headings."
  (let ((start (point)))
    (insert text)
    ;; Convert/fontify headings FIRST and isolate them with blank lines, so the
    ;; subsequent fill never merges a heading into an adjacent paragraph.
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^ *\\(=\\{2,6\\}\\) *\\(.*?\\) *=\\{2,6\\} *$" nil t)
        (let* ((level (length (match-string 1)))
               (head (match-string 2))
               (face (if (<= level 2) 'world-cup-summary-title 'bold)))
          (replace-match
           (concat "\n" (propertize head 'face face 'world-cup-heading t) "\n")
           t t))))
    ;; Fill only the non-heading paragraphs.
    (let ((fill-column world-cup-summary-fill-column)
          (left-margin 1))
      (save-excursion
        (goto-char start)
        (while (not (eobp))
          (if (get-text-property (point) 'world-cup-heading)
              (forward-line 1)
            (let ((p-start (point)))
              (forward-paragraph)
              (fill-region-as-paragraph p-start (point))
              (forward-line 1))))))))

(defun world-cup--summary-insert (summary body image)
  "Insert SUMMARY into the current buffer using BODY as text and IMAGE if non-nil."
  (let* ((title (alist-get 'title summary))
         (desc (alist-get 'description summary))
         (url (let ((cu (alist-get 'content_urls summary)))
                (alist-get 'page (alist-get 'desktop cu)))))
    (insert "\n")
    (when title
      (insert " " (propertize title 'face 'world-cup-summary-title) "\n\n"))
    (when (and desc (stringp desc) (not (string-empty-p desc)))
      (insert " " (propertize desc 'face 'world-cup-meta) "\n\n"))
    (when image
      (insert " ")
      (insert-image image)
      (insert "\n\n"))
    (when (and body (stringp body))
      (world-cup--insert-article-body body)
      (insert "\n"))
    (when url
      (insert "\n " (propertize url 'face 'link
                                'world-cup-url url
                                'help-echo "RET: open full article")
              "\n"))))

(defvar world-cup-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `world-cup-summary-mode'.")

(define-derived-mode world-cup-summary-mode special-mode "WC-Wiki"
  "Major mode for a Wikipedia article summary.")

(defvar-local world-cup-summary-url nil
  "URL of the full article shown in the current summary buffer.")
(defvar-local world-cup-summary--data nil
  "Summary alist backing the current buffer.")
(defvar-local world-cup-summary--full nil
  "Non-nil when the buffer shows the full article rather than the summary.")
(defvar-local world-cup-summary--body-cache nil
  "Cached full-text article extract for this buffer.")
(defvar-local world-cup-summary--image-cache 'unset
  "Cached thumbnail image (or nil); `unset' means not yet fetched.")

(defun world-cup-summary-browse-url ()
  "Open the full Wikipedia article for the current summary buffer."
  (interactive)
  (let ((url (or (get-text-property (point) 'world-cup-url)
                 world-cup-summary-url)))
    (if url (browse-url url)
      (user-error "No article URL available"))))

(defun world-cup-summary--render ()
  "Render the current summary buffer from its buffer-local state."
  (let* ((inhibit-read-only t)
         (summary world-cup-summary--data)
         (body (if world-cup-summary--full
                   (or world-cup-summary--body-cache
                       (setq world-cup-summary--body-cache
                             (world-cup--wikipedia-extract (alist-get 'title summary)))
                       (alist-get 'extract summary))
                 (alist-get 'extract summary))))
    (when (eq world-cup-summary--image-cache 'unset)
      (setq world-cup-summary--image-cache
            (let ((thumb (alist-get 'thumbnail summary)))
              (and thumb (world-cup--fetch-image
                          (alist-get 'source thumb)
                          :max-width world-cup-summary-image-max-width
                          :max-height world-cup-summary-image-max-width)))))
    (erase-buffer)
    (world-cup--summary-insert summary body world-cup-summary--image-cache)
    (insert "\n "
            (propertize (if world-cup-summary--full
                            "[TAB] show summary only"
                          "[TAB] load full article")
                        'face 'world-cup-meta)
            "\n")
    (goto-char (point-min))))

(defun world-cup-summary-toggle-detail ()
  "Toggle between the short summary and the full Wikipedia article."
  (interactive)
  (unless (derived-mode-p 'world-cup-summary-mode)
    (user-error "Not in a World Cup Wikipedia buffer"))
  (when (and (not world-cup-summary--full) (null world-cup-summary--body-cache))
    (message "Fetching full article..."))
  (setq world-cup-summary--full (not world-cup-summary--full))
  (world-cup-summary--render))

(defun world-cup--show-summary (summary)
  "Display SUMMARY in the dedicated `world-cup-summary-mode' buffer."
  (unless summary
    (user-error "No Wikipedia summary available"))
  (let ((buf (get-buffer-create "*World Cup: Wikipedia*")))
    (with-current-buffer buf
      (world-cup-summary-mode)
      (setq world-cup-summary--data summary
            world-cup-summary--full nil
            world-cup-summary--body-cache nil
            world-cup-summary--image-cache 'unset
            world-cup-summary-url
            (let ((cu (alist-get 'content_urls summary)))
              (alist-get 'page (alist-get 'desktop cu))))
      (world-cup-summary--render))
    (pop-to-buffer buf)))

(defib world-cup-player ()
  "Hyperbole implicit button: a World Cup player name in a roster.
Activating it (action key / mouse) opens that player's page.
The name text carries a `world-cup-player' text property (the player
alist) placed by the roster renderer."
  (when (derived-mode-p 'world-cup-team-mode)
    (let ((player (get-text-property (point) 'world-cup-player)))
      (when player
        (let* ((pos (point))
               (start (if (and (> pos (point-min))
                               (get-text-property (1- pos) 'world-cup-player))
                          (previous-single-property-change
                           pos 'world-cup-player)
                        pos))
               (end (or (next-single-property-change
                         pos 'world-cup-player)
                        (point-max))))
          (ibut:label-set (alist-get 'name player) start end)
          (hact 'world-cup-display-player player world-cup-team))))))

;;;; Wikipedia article cache (query -> resolved article title)

(defcustom world-cup-wikipedia-cache-file
  (expand-file-name "world-cup-wikipedia-cache.eld" world-cup-data-directory)
  "File persisting resolved Wikipedia article titles keyed by search query."
  :type 'file :group 'world-cup)

(defvar world-cup--wikipedia-cache nil
  "Hash table mapping a search query to a resolved Wikipedia article title.
Nil until first loaded; access via `world-cup--wikipedia-cache-table'.")

(defun world-cup--wikipedia-cache-load ()
  "Load and return the cache hash table from `world-cup-wikipedia-cache-file'."
  (let ((table (make-hash-table :test 'equal)))
    (when (file-readable-p world-cup-wikipedia-cache-file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents world-cup-wikipedia-cache-file)
          (dolist (cell (read (current-buffer)))
            (puthash (car cell) (cdr cell) table)))))
    table))

(defun world-cup--wikipedia-cache-table ()
  "Return the cache hash table, loading it from disk on first use."
  (or world-cup--wikipedia-cache
      (setq world-cup--wikipedia-cache (world-cup--wikipedia-cache-load))))

(defun world-cup--wikipedia-cache-save ()
  "Persist the cache to `world-cup-wikipedia-cache-file'."
  (let ((alist nil))
    (maphash (lambda (k v) (push (cons k v) alist))
             (world-cup--wikipedia-cache-table))
    (ignore-errors
      (with-temp-file world-cup-wikipedia-cache-file
        (let ((print-length nil) (print-level nil))
          (prin1 (sort alist (lambda (a b) (string< (car a) (car b))))
                 (current-buffer))
          (insert "\n"))))))

(defun world-cup--wikipedia-cache-get (query)
  "Return the cached article title for QUERY, or nil."
  (gethash query (world-cup--wikipedia-cache-table)))

(defun world-cup--wikipedia-cache-put (query title)
  "Cache TITLE as the resolved article for QUERY and persist."
  (puthash query title (world-cup--wikipedia-cache-table))
  (world-cup--wikipedia-cache-save))

(defun world-cup--wikipedia-cache-remove (query)
  "Drop the cached article for QUERY and persist."
  (remhash query (world-cup--wikipedia-cache-table))
  (world-cup--wikipedia-cache-save))

;;;###autoload
(defun world-cup-wikipedia-uncache (&optional query)
  "Remove the cached Wikipedia article for QUERY (e.g. a player name).
Interactively, choose among the currently cached entries.  The next lookup
for that query will prompt with a fresh search again."
  (interactive)
  (let* ((table (world-cup--wikipedia-cache-table))
         (cands nil))
    (maphash (lambda (k v)
               (push (cons (format "%s  \u2192  %s" k v) k) cands))
             table)
    (cond
     ((and (null query) (null cands))
      (message "World Cup: Wikipedia cache is empty"))
     (t
      (let ((key (or query
                     (cdr (assoc (completing-read "Bust cache for: "
                                                  (mapcar #'car cands) nil t)
                                 cands)))))
        (if (and key (world-cup--wikipedia-cache-get key))
            (progn (world-cup--wikipedia-cache-remove key)
                   (message "World Cup: busted Wikipedia cache for %s" key))
          (message "World Cup: no cache entry for %s" key)))))))

;;;; Wikipedia article resolution + lookup

(defun world-cup--resolve-article (query)
  "Resolve QUERY to a Wikipedia article title.
Use the cache when present; otherwise run a consult search and cache the
chosen title.  Return a title string, or the symbol `none' if the search
returned no results."
  (or (world-cup--wikipedia-cache-get query)
      (let ((results (world-cup--wikipedia-search query)))
        (if (null results)
            'none
          (let* ((cands (mapcar (lambda (r) (cons (plist-get r :title) r)) results))
                 (titles (mapcar #'car cands))
                 (annotate
                  (lambda (cand)
                    (when-let* ((r (cdr (assoc cand cands)))
                                (d (plist-get r :desc))
                                ((not (string-empty-p d))))
                      (concat "  " (propertize d 'face 'world-cup-meta)))))
                 (prompt (format "Wikipedia (%s): " query))
                 (choice
                  (if (fboundp 'consult--read)
                      (consult--read titles
                                     :prompt prompt
                                     :category 'world-cup-wikipedia
                                     :require-match t
                                     :sort nil
                                     :annotate annotate)
                    (let ((completion-extra-properties
                           (list :annotation-function annotate)))
                      (completing-read prompt titles nil t)))))
            (world-cup--wikipedia-cache-put query choice)
            choice)))))

(defun world-cup--lookup-summary (query)
  "Return (TITLE . SUMMARY) for QUERY; TITLE is `none' if no results.
A stale cached title (whose summary cannot be fetched) is dropped and the
query is re-resolved once."
  (let ((title (world-cup--resolve-article query)))
    (if (eq title 'none)
        (cons 'none nil)
      (let ((summary (world-cup--wikipedia-summary title)))
        (if summary
            (cons title summary)
          (world-cup--wikipedia-cache-remove query)
          (let ((title2 (world-cup--resolve-article query)))
            (if (eq title2 'none)
                (cons 'none nil)
              (cons title2 (world-cup--wikipedia-summary title2)))))))))

;;;###autoload
(defun world-cup-wikipedia-lookup (&optional query)
  "Show a Wikipedia summary (with image) for QUERY in a dedicated buffer.
Interactively, prompt for the search string.  The first lookup for a QUERY
runs a consult search and caches the chosen article title; subsequent lookups
skip the search.  Use \[world-cup-wikipedia-uncache] to clear a bad entry."
  (interactive)
  (let* ((query (or query (read-string "Wikipedia search: ")))
         (res (world-cup--lookup-summary query)))
    (if (or (eq (car res) 'none) (null (cdr res)))
        (user-error "No Wikipedia results for %s" query)
      (world-cup--show-summary (cdr res)))))

;;;; YouTube preview (consult search + mpv streaming)

(defcustom world-cup-yt-dlp-program "yt-dlp"
  "Path to the yt-dlp executable used for YouTube searches."
  :type 'string :group 'world-cup)

(defcustom world-cup-mpv-program "mpv"
  "Path to the mpv executable used to stream videos."
  :type 'string :group 'world-cup)

(defcustom world-cup-mpv-args nil
  "Extra arguments passed to mpv before the video URL."
  :type '(repeat string) :group 'world-cup)

(defcustom world-cup-youtube-search-count 15
  "Number of YouTube results to fetch per search."
  :type 'integer :group 'world-cup)

(defun world-cup--format-duration (secs)
  "Format SECS (a number) as M:SS or H:MM:SS."
  (when (numberp secs)
    (let* ((s (round secs))
           (h (/ s 3600))
           (m (/ (% s 3600) 60))
           (sec (% s 60)))
      (if (> h 0)
          (format "%d:%02d:%02d" h m sec)
        (format "%d:%02d" m sec)))))

(defun world-cup--youtube-search (query &optional n)
  "Search YouTube for QUERY via yt-dlp, returning a list of result plists.
Each plist has :title, :id, :channel, :duration and :views."
  (unless (executable-find world-cup-yt-dlp-program)
    (user-error "Cannot find yt-dlp (%s)" world-cup-yt-dlp-program))
  (with-temp-buffer
    (let ((status (call-process
                   world-cup-yt-dlp-program nil t nil
                   "--flat-playlist" "--dump-json" "--ignore-errors"
                   (format "ytsearch%d:%s"
                           (or n world-cup-youtube-search-count) query))))
      (goto-char (point-min))
      (let (results)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (string-empty-p line)
              (ignore-errors
                (let ((d (json-parse-string line :object-type 'alist
                                            :null-object nil :false-object nil)))
                  (push (list :title (alist-get 'title d)
                              :id (alist-get 'id d)
                              :channel (or (alist-get 'channel d)
                                           (alist-get 'uploader d))
                              :duration (alist-get 'duration d)
                              :views (alist-get 'view_count d))
                        results)))))
          (forward-line 1))
        (when (and (null results) (/= status 0))
          (user-error "yt-dlp search failed (exit %s)" status))
        (nreverse results)))))

(defun world-cup--mpv-play (url)
  "Stream URL with mpv in a detached process."
  (unless (executable-find world-cup-mpv-program)
    (user-error "Cannot find mpv (%s)" world-cup-mpv-program))
  (let ((proc (apply #'start-process "world-cup-mpv" nil
                     world-cup-mpv-program
                     (append world-cup-mpv-args (list url)))))
    (set-process-query-on-exit-flag proc nil)
    (message "mpv: streaming %s" url)
    proc))

;;;###autoload
(defun world-cup-youtube-watch (&optional query)
  "Search YouTube for QUERY via consult and stream the chosen video with mpv.
When called from a Hyperbole fixture button, QUERY is the match preview string.
Interactively, prompt for the search string."
  (interactive)
  (let* ((query (or query (read-string "YouTube search: ")))
         (results (world-cup--youtube-search query)))
    (unless results
      (user-error "No YouTube results for %s" query))
    (let* ((cands
            (mapcar (lambda (r)
                      (cons (or (plist-get r :title) (plist-get r :id)) r))
                    results))
           (titles (mapcar #'car cands))
           (annotate
            (lambda (cand)
              (when-let* ((r (cdr (assoc cand cands))))
                (let ((dur (world-cup--format-duration (plist-get r :duration)))
                      (ch (plist-get r :channel))
                      (views (plist-get r :views)))
                  (concat
                   (when dur (propertize (format "  [%s]" dur)
                                         'face 'font-lock-constant-face))
                   (when ch (propertize (format "  %s" ch)
                                        'face 'world-cup-meta))
                   (when (numberp views)
                     (propertize (format "  %s views" views)
                                 'face 'world-cup-meta)))))))
           (prompt (format "YouTube (%s): " query))
           (choice
            (if (fboundp 'consult--read)
                (consult--read titles
                               :prompt prompt
                               :category 'world-cup-youtube
                               :require-match t
                               :sort nil
                               :annotate annotate)
              (let ((completion-extra-properties
                     (list :annotation-function annotate)))
                (completing-read prompt titles nil t))))
           (r (cdr (assoc choice cands))))
      (when r
        (world-cup--mpv-play
         (format "https://www.youtube.com/watch?v=%s" (plist-get r :id)))))))

;;;; Player formatting helpers

(defun world-cup--age (dob)
  "Return the integer age in years for DOB (a DD/MM/YYYY string), or nil."
  (when (and dob (string-match
                  "\\`\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{4\\}\\)\\'" dob))
    (let* ((d (string-to-number (match-string 1 dob)))
           (m (string-to-number (match-string 2 dob)))
           (y (string-to-number (match-string 3 dob)))
           (now (decode-time))
           (cd (nth 3 now)) (cm (nth 4 now)) (cy (nth 5 now))
           (age (- cy y)))
      (when (or (< cm m) (and (= cm m) (< cd d)))
        (setq age (1- age)))
      age)))

(defun world-cup--height-imperial (cm)
  "Convert CM (an integer) to a feet/inches string like 6'1\", or \"\"."
  (if (and cm (> cm 0))
      (let* ((inch (round (/ cm 2.54)))
             (ft (/ inch 12))
             (in (% inch 12)))
        (format "%d'%d\"" ft in))
    ""))

(defun world-cup--club-label (player)
  "Return PLAYER's club formatted as \"Club Name (NAT)\"."
  (let ((club (or (alist-get 'club player) ""))
        (nat (alist-get 'club_country player)))
    (if (and nat (not (string-empty-p nat)))
        (format "%s (%s)" club nat)
      club)))

;;;; Roster section

(defconst world-cup--name-col 40
  "Absolute column where the post-name fields begin in a roster row.")

(defun world-cup--insert-player (player)
  "Insert one PLAYER row, with the name as a Hyperbole implicit button."
  (let* ((num (alist-get 'number player))
         (pos (or (alist-get 'position player) ""))
         (name (or (alist-get 'name player) ""))
         (club (world-cup--pad (world-cup--club-label player) 32))
         (age (world-cup--age (alist-get 'dob player)))
         (ht (world-cup--height-imperial (alist-get 'height_cm player))))
    (insert "  ")
    ;; The name carries a text property recognized by the `world-cup-player'
    ;; Hyperbole implicit button type (see `defib' above).
    (insert (propertize name
                        'world-cup-player player
                        'face 'world-cup-player-link
                        'help-echo "Action key: open this player's page"))
    ;; Asterisk if the player is on the FOX Sports Top 100.
    (when-let ((fox (world-cup-fox-ranking
                     (and world-cup-team (world-cup-team-code world-cup-team))
                     num)))
      (insert (propertize "*" 'face 'world-cup-rank
                          'help-echo (format "FOX Sports Top 100: #%d"
                                             (alist-get 'rank fox)))))
    (insert (make-string (max 1 (- world-cup--name-col (current-column))) ?\s))
    (insert (format "%-4s%-34s%-5s%-7s%s\n"
                    (propertize pos 'face (world-cup--pos-face pos))
                    club
                    (if age (number-to-string age) "?")
                    ht
                    (format "#%d" num)))))

(defun world-cup--insert-roster (team)
  "Insert the Squad section for TEAM."
  (let ((players (world-cup-team-players team)))
    (magit-insert-section (world-cup-roster)
      (world-cup--heading (format "Squad (%d players)" (length players)))
      (insert "  "
              (propertize
               (format "%-38s%-4s%-34s%-5s%-7s%s\n"
                       "Name" "Pos" "Club (Nat)" "Age" "Ht" "#")
               'face 'world-cup-column-header))
      (dolist (p players)
        (world-cup--insert-player p))
      (insert "\n"))))

;;;; Fixtures section

(defun world-cup--opponent (team match)
  "Return (OPP-LABEL . HOME-P) for TEAM in MATCH.
HOME-P is non-nil when TEAM is team_a."
  (let ((code (world-cup-team-code team)))
    (if (equal (alist-get 'team_a_code match) code)
        (cons (alist-get 'team_b match) t)
      (cons (alist-get 'team_a match) nil))))

(defib world-cup-fixture ()
  "Hyperbole implicit button: a fixture row in a roster buffer.
Activating it opens that game's page.  The row carries a `world-cup-fixture'
text property (the match alist) placed by the fixtures renderer."
  (when (derived-mode-p 'world-cup-team-mode)
    (let ((match (get-text-property (point) 'world-cup-fixture)))
      (when match
        (let* ((pos (point))
               (start (if (and (> pos (point-min))
                               (get-text-property (1- pos) 'world-cup-fixture))
                          (previous-single-property-change
                           pos 'world-cup-fixture)
                        pos))
               (end (or (next-single-property-change
                         pos 'world-cup-fixture)
                        (point-max))))
          (ibut:label-set (format "%s vs %s"
                                  (alist-get 'team_a match)
                                  (alist-get 'team_b match))
                          start end)
          (hact 'world-cup-display-game match))))))

(defun world-cup--insert-fixture (team match)
  "Insert one MATCH row for TEAM inside a `magit-section'.
The row is a `world-cup-fixture' Hyperbole implicit button (opens the game)."
  (pcase-let* ((`(,opp . ,home-p) (world-cup--opponent team match))
               (grp (alist-get 'group match))
               (label (if grp (format "Grp %s" grp)
                        (or (alist-get 'stage match) "")))
               (result (world-cup--fixture-result-tag team match))
               (line (format "  %s %5s  %-7s  %s %-24s  %s"
                             (world-cup--local-date match)
                             (world-cup--local-time match)
                             label
                             (if home-p "vs" "@ ")
                             (propertize (world-cup--pad opp 24)
                                         'face 'world-cup-player-link)
                             (propertize
                              (format "%s, %s"
                                      (alist-get 'venue match)
                                      (alist-get 'city match))
                              'face 'world-cup-meta))))
    (magit-insert-section (world-cup-match match)
      (magit-insert-heading
        (propertize (if result (concat line "  " result) line)
                    'world-cup-fixture match
                    'help-echo "Action key: open game page"))
      (when-let ((note (world-cup-match-note match)))
        (let ((start (point)))
          (insert "       \u21b3 " (propertize note 'face 'world-cup-note) "\n")
          (let ((fill-column 84) (left-margin 9) (fill-prefix "         "))
            (fill-region start (point))))))))

(defun world-cup--fixture-result-tag (team match)
  "Return a propertized W/D/L score tag for TEAM in MATCH, or nil if unplayed."
  (when-let ((r (world-cup-match-result match)))
    (let* ((code (world-cup-team-code team))
           (hs (alist-get 'home_score r)) (as (alist-get 'away_score r))
           (home-p (equal (alist-get 'home r) code))
           (gf (if home-p hs as)) (ga (if home-p as hs))
           (live (not (equal (alist-get 'status r) "post")))
           (outcome (cond ((null gf) "") ((> gf ga) "W") ((< gf ga) "L") (t "D")))
           (face (cond (live 'world-cup-meta)
                       ((equal outcome "W") 'world-cup-position-fw)
                       ((equal outcome "L") 'error)
                       (t 'warning))))
      (propertize (format "%s %d-%d%s" outcome gf ga (if live " \u25cf" ""))
                  'face face))))

(defun world-cup--insert-fixtures (team)
  "Insert the Fixtures section for TEAM."
  (let ((matches (world-cup-team-matches team)))
    (magit-insert-section (world-cup-fixtures)
      (world-cup--heading (format "Fixtures (%d)" (length matches)))
      (if (null matches)
          (insert (propertize "  No scheduled matches found.\n"
                              'face 'world-cup-meta))
        (insert (propertize
                 (format "  %-10s %5s  %-7s  %-27s  %s\n"
                         "Date" (world-cup--tz-abbrev (car matches))
                         "Stage" "Opponent" "Venue")
                 'face 'world-cup-column-header))
        (dolist (m matches)
          (world-cup--insert-fixture team m)))
      (insert "\n"))))

;;;; Major mode + buffer

(defvar-local world-cup-team nil
  "The team alist displayed in the current `world-cup-team-mode' buffer.")

(defvar world-cup-team-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    map)
  "Keymap for `world-cup-team-mode'.")

(define-derived-mode world-cup-team-mode magit-section-mode "WC-Team"
  "Major mode showing a World Cup team's fixtures and squad.
\\{world-cup-team-mode-map}")

(defun world-cup--render (team)
  "Render TEAM's fixtures and squad into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq world-cup-team team)
    (setq header-line-format
          (concat " "
                  (propertize (world-cup-team-name team) 'face 'world-cup-title)
                  "  "
                  (propertize (format "[%s]" (world-cup-team-code team))
                              'face 'world-cup-code)
                  (propertize (format "   Coach: %s"
                                      (or (world-cup-team-coach-name team) "?"))
                              'face 'world-cup-meta)))
    (magit-insert-section (world-cup-team-root)
      (world-cup--insert-analysis team)
      (world-cup--insert-power-rankings team)
      (world-cup--insert-history team)
      (world-cup--insert-fixtures team)
      (world-cup--insert-roster team))
    (goto-char (point-min))))

(defun world-cup--insert-analysis-field (label text)
  "Insert a labelled, filled analysis paragraph for LABEL and TEXT."
  (when (and text (stringp text) (not (string-empty-p text)))
    (insert "  " (propertize (concat label ": ") 'face 'world-cup-label))
    (let ((start (point)))
      (insert text)
      (let ((fill-column 80) (left-margin 4) (fill-prefix "    "))
        (fill-region start (point)))
      (insert "\n"))))

(defun world-cup--insert-history (team)
  "Insert a one-line World Cup history summary for TEAM, if available."
  (when-let ((h (world-cup-team-history team)))
    (let* ((apps (alist-get 'appearances h))
           (titles (alist-get 'titles h))
           (ru (alist-get 'runners_up h))
           (best (alist-get 'best_finish h))
           (last-y (alist-get 'last_appearance h))
           (last-r (alist-get 'last_result h))
           (parts nil))
      (push (format "%d appearance%s" apps (if (= apps 1) "" "s")) parts)
      (cond
       ((> (length titles) 0)
        (push (format "%d title%s (%s)" (length titles)
                      (if (= (length titles) 1) "" "s")
                      (mapconcat #'number-to-string titles ", ")) parts))
       ((> (length ru) 0)
        (push (format "runners-up %s" (mapconcat #'number-to-string ru ", ")) parts)))
      (when (and best (= (length titles) 0) (= (length ru) 0))
        (push (format "best: %s" best) parts))
      (push (if (and last-y last-r)
                (format "last: %d (%s)" last-y last-r)
              "first World Cup")
            parts)
      (magit-insert-section (world-cup-history)
        (world-cup--heading "History")
        (insert "  "
                (propertize (mapconcat #'identity (nreverse parts) "  \u00b7  ")
                            'face 'world-cup-meta)
                "\n\n")))))

(defun world-cup--insert-analysis (team)
  "Insert the foldable Analysis section for TEAM, if available."
  (when-let ((a (world-cup-team-analysis team)))
    (magit-insert-section (world-cup-analysis)
      (world-cup--heading "Analysis")
      (world-cup--insert-analysis-field "Narrative" (alist-get 'narrative a))
      (world-cup--insert-analysis-field "Key players" (alist-get 'key_players a))
      (world-cup--insert-analysis-field "Hinges on" (alist-get 'hinges_on a))
      (let ((notes (alist-get 'notes a)))
        (when notes
          (insert "  " (propertize "Notes:" 'face 'world-cup-label) "\n")
          (dolist (n notes)
            (let ((start (point)))
              (insert "    \u2022 " n)
              (let ((fill-column 80) (left-margin 6) (fill-prefix "      "))
                (fill-region start (point)))
              (insert "\n")))))
      (insert "\n"))))

(defun world-cup--insert-power-rankings (team)
  "Insert the Power Rankings section for TEAM, if available.
Each source is its own collapsible sub-section (TAB toggles the write-up)."
  (when-let ((prs (world-cup-team-power-rankings team)))
    (let* ((ranks (mapcar (lambda (r) (alist-get 'rank r)) prs))
           (avg (/ (apply #'+ ranks) (float (length ranks)))))
      (magit-insert-section (world-cup-power-rankings)
        (world-cup--heading
         (format "Power Rankings  (avg #%.1f across %d sources)"
                 avg (length prs)))
        (dolist (r prs)
          ;; Trailing t => each source's write-up starts collapsed.
          (magit-insert-section (world-cup-power-rank r t)
            (magit-insert-heading
              (concat "  "
                      (propertize (format "#%d" (alist-get 'rank r))
                                  'face 'world-cup-rank)
                      "  "
                      (propertize (alist-get 'source r) 'face 'world-cup-label)))
            (let ((start (point)))
              (insert "      "
                      (propertize (alist-get 'description r) 'face 'world-cup-quote)
                      "\n")
              (let ((fill-column 88) (fill-prefix "      "))
                (fill-region start (point))))))
        (insert "\n")))))

(defun world-cup-team-revert ()
  "Reload data from disk and re-render the current team buffer."
  (interactive)
  (let ((code (world-cup-team-code world-cup-team)))
    (world-cup-load-data t)
    (world-cup--render (world-cup--find-team-by-code code))))

(defun world-cup-display-team (team)
  "Display TEAM's fixtures and squad in a `world-cup-team-mode' buffer."
  (let ((buffer (get-buffer-create
                 (format "*World Cup: %s*" (world-cup-team-name team)))))
    (with-current-buffer buffer
      (world-cup-team-mode)
      (world-cup--render team))
    (pop-to-buffer buffer)))

;;;###autoload
(defun world-cup-consult-team ()
  "Search through World Cup teams and show the selected team's page.
The buffer shows collapsible Fixtures and Squad sections."
  (interactive)
  (world-cup-display-team (world-cup--read-team)))

;;;; Player buffer

(defcustom world-cup-web-search-url-format "https://duckduckgo.com/?q=%s"
  "Format string for web searches; %s is replaced by the URL-encoded query."
  :type 'string :group 'world-cup)

(defvar world-cup-player-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `world-cup-player-mode'.")

(define-derived-mode world-cup-player-mode special-mode "WC-Player"
  "Major mode for a single World Cup player's page.")

(defvar-local world-cup-player--player nil "Player alist for this buffer.")
(defvar-local world-cup-player--team nil "Team alist for this buffer's player.")
(defvar-local world-cup-player--query nil "Wikipedia search query (player name).")
(defvar-local world-cup-player--title nil "Resolved Wikipedia article title.")
(defvar-local world-cup-player--summary nil "Wikipedia summary alist, or nil.")
(defvar-local world-cup-player--no-results nil "Non-nil when no article was found.")
(defvar-local world-cup-player--full nil "Non-nil when showing the full article.")
(defvar-local world-cup-player--body-cache nil "Cached full-text article extract.")
(defvar-local world-cup-player--image-cache 'unset "Cached thumbnail image.")

(defun world-cup-player--guard ()
  (unless (derived-mode-p 'world-cup-player-mode)
    (user-error "Not in a World Cup player buffer")))

(defun world-cup-player--web-query ()
  "Return the web-search query string for the buffer's player."
  (format "%s %s footballer"
          (alist-get 'name world-cup-player--player)
          (world-cup-team-name world-cup-player--team)))

(defun world-cup-player--web-url ()
  (format world-cup-web-search-url-format
          (url-hexify-string (world-cup-player--web-query))))

(defun world-cup-player--insert-stats ()
  "Insert the basic stats block for the buffer's player."
  (let* ((p world-cup-player--player)
         (team world-cup-player--team)
         (pos (or (alist-get 'position p) ""))
         (age (world-cup--age (alist-get 'dob p)))
         (dob (or (alist-get 'dob p) "?"))
         (cm (alist-get 'height_cm p))
         (ht (world-cup--height-imperial cm)))
    (insert " " (propertize (or (alist-get 'name p) "")
                            'face 'world-cup-summary-title)
            "\n\n")
    (dolist (row (list
                  (cons "Team" (format "%s [%s]" (world-cup-team-name team)
                                       (world-cup-team-code team)))
                  (cons "Position" (concat pos (pcase pos
                                                 ("GK" "  (Goalkeeper)")
                                                 ("DF" "  (Defender)")
                                                 ("MF" "  (Midfielder)")
                                                 ("FW" "  (Forward)") (_ ""))))
                  (cons "Squad #" (number-to-string (alist-get 'number p)))
                  (cons "Club" (world-cup--club-label p))
                  (cons "Age" (if age (format "%d   (DOB %s)" age dob) dob))
                  (cons "Height" (if cm (format "%s   (%s cm)" ht cm) "?"))))
      (insert (format " %-10s %s\n"
                      (propertize (concat (car row) ":")
                                  'face 'world-cup-label)
                      (cdr row))))))

(defun world-cup-player--insert-help ()
  (insert (propertize " Press ? for actions" 'face 'world-cup-meta) "\n"))

(defun world-cup-player--insert-tournament ()
  "Insert this tournament's stats for the buffer's player, if they've played."
  (when-let ((s (world-cup-player-stats world-cup-player--team
                                        world-cup-player--player)))
    (when (> (or (alist-get 'apps s) 0) 0)
      (insert "\n " (propertize "\u2014 This World Cup \u2014" 'face 'world-cup-meta) "\n")
      (dolist (row (list
                    (cons "Matches" (format "%d  (%d start%s, %d off the bench)"
                                            (alist-get 'apps s) (alist-get 'starts s)
                                            (if (= (alist-get 'starts s) 1) "" "s")
                                            (alist-get 'subs s)))
                    (cons "Minutes" (number-to-string (alist-get 'minutes s)))
                    (cons "Goals" (number-to-string (alist-get 'goals s)))
                    (cons "Assists" (number-to-string (alist-get 'assists s)))
                    (cons "Cards" (format "%d yellow, %d red"
                                          (alist-get 'yellow s) (alist-get 'red s)))))
        (insert " " (propertize (format "%-9s" (concat (car row) ":"))
                                'face 'world-cup-label)
                " " (cdr row) "\n"))
      (when-let ((ms (alist-get 'matches s)))
        (insert " " (propertize "Matches:" 'face 'world-cup-label) "\n")
        (dolist (m (sort (copy-sequence ms)
                         (lambda (a b) (< (or (alist-get 'num a) 999)
                                          (or (alist-get 'num b) 999)))))
          (let* ((opp (alist-get 'opp m))
                 (oppname (or (and opp (world-cup--find-team-by-code opp)
                                   (world-cup-team-name
                                    (world-cup--find-team-by-code opp)))
                              opp "?"))
                 (res (alist-get 'res m))
                 (face (cond ((equal res "W") 'world-cup-position-fw)
                             ((equal res "L") 'error) (t 'warning)))
                 (g (alist-get 'g m)) (a (alist-get 'a m)))
            (insert "   "
                    (propertize (format "%s %d-%d" res (alist-get 'gf m) (alist-get 'ga m))
                                'face face)
                    (format "  vs %-22s %-5s %3d'"
                            (world-cup--pad oppname 22)
                            (if (alist-get 'started m) "start" "sub")
                            (alist-get 'min m))
                    (concat (if (> g 0) (propertize (format "  %d\u26bd" g)
                                                    'face 'world-cup-position-fw) "")
                            (if (> a 0) (format "  %dA" a) ""))
                    "\n")))))))

(defun world-cup-player--render ()
  "Render the player buffer from its buffer-local state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (world-cup-player--insert-stats)
    (world-cup-player--insert-tournament)
    (when-let ((fox (world-cup-player-fox-ranking
                     world-cup-player--team world-cup-player--player)))
      (insert "\n " (propertize (format "\u2014 FOX Sports Top 100: #%d \u2014"
                                        (alist-get 'rank fox))
                                'face 'world-cup-meta) "\n")
      (let ((start (point)))
        (insert " " (propertize (alist-get 'summary fox) 'face 'world-cup-quote) "\n")
        (let ((fill-column 80) (left-margin 1) (fill-prefix " "))
          (fill-region start (point)))))
    (insert "\n " (propertize "\u2014 Wikipedia \u2014"
                              'face 'world-cup-meta) "\n")
    (cond
     (world-cup-player--no-results
      (insert "\n " (propertize "<no wikipedia results>" 'face 'warning) "\n"))
     (world-cup-player--summary
      (when (eq world-cup-player--image-cache 'unset)
        (setq world-cup-player--image-cache
              (let ((thumb (alist-get 'thumbnail world-cup-player--summary)))
                (and thumb (world-cup--fetch-image
                            (alist-get 'source thumb)
                            :max-width world-cup-summary-image-max-width
                            :max-height world-cup-summary-image-max-width)))))
      (let ((body (if world-cup-player--full
                      (or world-cup-player--body-cache
                          (setq world-cup-player--body-cache
                                (world-cup--wikipedia-extract world-cup-player--title))
                          (alist-get 'extract world-cup-player--summary))
                    (alist-get 'extract world-cup-player--summary))))
        (world-cup--summary-insert world-cup-player--summary body
                                   world-cup-player--image-cache))
      (insert "\n " (propertize (if world-cup-player--full
                                    "[TAB] show summary only"
                                  "[TAB] load full article")
                                'face 'world-cup-meta) "\n"))
     (t
      (insert "\n " (propertize "Looking up Wikipedia\u2026"
                                'face 'world-cup-meta) "\n")))
    (insert "\n")
    (world-cup-player--insert-help)
    (goto-char (point-min))))

(defun world-cup-player--load-wikipedia (buffer)
  "Resolve and load the Wikipedia article for BUFFER's player, then re-render."
  (let ((query (with-current-buffer buffer world-cup-player--query)))
    (let ((res (world-cup--lookup-summary query)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (or (eq (car res) 'none) (null (cdr res)))
              (setq world-cup-player--no-results t
                    world-cup-player--summary nil)
            (setq world-cup-player--no-results nil
                  world-cup-player--title (car res)
                  world-cup-player--summary (cdr res)
                  world-cup-player--full nil
                  world-cup-player--body-cache nil
                  world-cup-player--image-cache 'unset))
          (world-cup-player--render))))))

(defun world-cup-player-youtube-highlights ()
  "Search YouTube for the player's highlights and stream the choice with mpv."
  (interactive)
  (world-cup-player--guard)
  (world-cup-youtube-watch
   (format "%s highlights" (alist-get 'name world-cup-player--player))))

(defun world-cup-player-web-xwidget ()
  "Run a web search for the player in an xwidget-webkit buffer."
  (interactive)
  (world-cup-player--guard)
  (unless (fboundp 'xwidget-webkit-browse-url)
    (user-error "This Emacs was not built with xwidget support"))
  (xwidget-webkit-browse-url (world-cup-player--web-url)))

(defun world-cup-player-web-browser ()
  "Run a web search for the player in the external browser."
  (interactive)
  (world-cup-player--guard)
  (browse-url (world-cup-player--web-url)))

(defun world-cup-player-toggle-detail ()
  "Toggle between the Wikipedia summary and the full article."
  (interactive)
  (world-cup-player--guard)
  (unless world-cup-player--summary
    (user-error "No Wikipedia article loaded"))
  (setq world-cup-player--full (not world-cup-player--full))
  (world-cup-player--render))

(defun world-cup-player-reload ()
  "Reload the Wikipedia article for the current player buffer."
  (interactive)
  (world-cup-player--guard)
  (world-cup-player--load-wikipedia (current-buffer)))

;;;###autoload
(defun world-cup-display-player (player team)
  "Show a buffer for PLAYER of TEAM: basic stats plus a Wikipedia summary.
On load, a consult search resolves which Wikipedia article to show (cached
after the first time); if nothing matches, <no wikipedia results> is shown."
  (let ((buf (get-buffer-create
              (format "*World Cup Player: %s*" (alist-get 'name player)))))
    (with-current-buffer buf
      (world-cup-player-mode)
      (setq world-cup-player--player player
            world-cup-player--team team
            world-cup-player--query (alist-get 'name player)
            world-cup-player--title nil
            world-cup-player--summary nil
            world-cup-player--no-results nil
            world-cup-player--full nil
            world-cup-player--body-cache nil
            world-cup-player--image-cache 'unset)
      (world-cup-player--render))
    (pop-to-buffer buf)
    (world-cup-player--load-wikipedia buf)))

;;;; Player search

(defun world-cup--player-candidates ()
  "Return an alist of (DISPLAY . (PLAYER . TEAM)) for every player.
DISPLAY includes the player's name and country so completion matches on
either."
  (let (cands)
    (dolist (team (world-cup-teams))
      (let ((tname (world-cup-team-name team)))
        (dolist (p (world-cup-team-players team))
          (push (cons (format "%-26s  %s  #%d"
                              (alist-get 'name p) tname (alist-get 'number p))
                      (cons p team))
                cands))))
    (nreverse cands)))

(defun world-cup--annotate-player (cands)
  "Return an annotation function for the player candidate alist CANDS."
  (lambda (cand)
    (when-let* ((pt (cdr (assoc cand cands)))
                (p (car pt)))
      (let ((pos (or (alist-get 'position p) ""))
            (age (world-cup--age (alist-get 'dob p))))
        (concat
         (propertize (format "  %-2s" pos) 'face (world-cup--pos-face pos))
         (propertize (format "  %s" (world-cup--club-label p))
                     'face 'world-cup-meta)
         (when age (propertize (format "  age %d" age)
                               'face 'world-cup-meta)))))))

;;;###autoload
(defun world-cup-consult-player ()
  "Search all World Cup players (by name or country) and look one up.
Selecting a player starts a Wikipedia search about them, exactly like the
implicit button on a roster name."
  (interactive)
  (let* ((cands (world-cup--player-candidates))
         (annotate (world-cup--annotate-player cands))
         (names (mapcar #'car cands))
         (choice
          (if (fboundp 'consult--read)
              (consult--read names
                             :prompt "World Cup player: "
                             :category 'world-cup-player
                             :require-match t
                             :sort t
                             :annotate annotate)
            (let ((completion-extra-properties
                   (list :annotation-function annotate)))
              (completing-read "World Cup player: " names nil t))))
         (pt (cdr (assoc choice cands)))
         (player (car pt))
         (team (cdr pt)))
    (when player
      (world-cup-display-player player team))))

;;;; Game (fixture) buffer

(defun world-cup--match-label (match)
  "Return a short stage label for MATCH (\"Grp X\" or the stage name)."
  (let ((grp (alist-get 'group match)))
    (if grp (format "Grp %s" grp) (or (alist-get 'stage match) ""))))

(defun world-cup--match-team (match side)
  "Return the team alist for SIDE (`a' or `b') of MATCH, or nil."
  (let ((code (alist-get (if (eq side 'a) 'team_a_code 'team_b_code) match))
        (name (alist-get (if (eq side 'a) 'team_a 'team_b) match)))
    (or (and code (world-cup--find-team-by-code code))
        (world-cup--find-team-by-name name))))

(defvar world-cup-game-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `world-cup-game-mode'.")

(define-derived-mode world-cup-game-mode special-mode "WC-Game"
  "Major mode for a single World Cup game's page.")

(defvar-local world-cup-game--match nil
  "Match alist backing the current game buffer.")

(defun world-cup-game--guard ()
  (unless (derived-mode-p 'world-cup-game-mode)
    (user-error "Not in a World Cup game buffer")))

(defun world-cup-game--teams ()
  "Return \"TEAM_A vs TEAM_B\" for the current game."
  (format "%s vs %s"
          (alist-get 'team_a world-cup-game--match)
          (alist-get 'team_b world-cup-game--match)))

(defun world-cup-game--web-query ()
  (format "world cup game %d %s"
          (alist-get 'match_number world-cup-game--match)
          (world-cup-game--teams)))

(defun world-cup-game--web-url ()
  (format world-cup-web-search-url-format
          (url-hexify-string (world-cup-game--web-query))))

(defun world-cup-game--insert-stats ()
  "Insert the basic stats block for the buffer's game."
  (let* ((m world-cup-game--match)
         (a (alist-get 'team_a m)) (b (alist-get 'team_b m))
         (ac (alist-get 'team_a_code m)) (bc (alist-get 'team_b_code m)))
    (insert " " (propertize (format "%s  vs  %s" a b)
                            'face 'world-cup-summary-title)
            "\n\n")
    (dolist (row (list
                  (cons "Match" (format "#%d" (alist-get 'match_number m)))
                  (cons "Stage" (let ((g (alist-get 'group m)))
                                  (if g (format "Group %s" g)
                                    (or (alist-get 'stage m) "?"))))
                  (cons "Date" (world-cup--local-date m))
                  (cons "Kickoff" (format "%s %s   (%s ET)"
                                          (world-cup--local-time m)
                                          (world-cup--tz-abbrev m)
                                          (alist-get 'time_et m)))
                  (cons "Home" (if ac (format "%s [%s]" a ac) a))
                  (cons "Away" (if bc (format "%s [%s]" b bc) b))
                  (cons "Venue" (or (alist-get 'venue m) "?"))
                  (cons "City" (format "%s, %s"
                                       (alist-get 'city m)
                                       (alist-get 'country m)))))
      (insert (format " %-9s %s\n"
                      (propertize (concat (car row) ":")
                                  'face 'world-cup-label)
                      (cdr row))))))

(defun world-cup-game--insert-result ()
  "Insert the live score, stats, goals and cards for the current game."
  (when-let ((r (world-cup-match-result world-cup-game--match)))
    (let* ((h (alist-get 'home r)) (a (alist-get 'away r))
           (hs (alist-get 'home_score r)) (as (alist-get 'away_score r))
           (stats (alist-get 'stats r)))
      (insert "\n ")
      (insert (propertize (format "%s %d \u2013 %d %s" h hs as a)
                          'face 'world-cup-title)
              "  "
              (propertize (format "(%s)" (or (alist-get 'detail r) ""))
                          'face 'world-cup-meta)
              "\n")
      ;; goals
      (dolist (g (alist-get 'goals r))
        (insert "   " (propertize "\u26bd " 'face 'world-cup-position-fw)
                (propertize (format "%-4s" (alist-get 'min g)) 'face 'world-cup-meta)
                (format "%s (%s)" (or (alist-get 'player g) "?") (alist-get 'code g))
                (cond ((alist-get 'own g) (propertize " OG" 'face 'world-cup-rank))
                      ((alist-get 'pen g) (propertize " pen" 'face 'world-cup-meta))
                      (t ""))
                "\n"))
      ;; cards
      (dolist (c (alist-get 'cards r))
        (insert "   "
                (propertize (if (equal (alist-get 'color c) "red") "\u25a0" "\u25a0")
                            'face (if (equal (alist-get 'color c) "red")
                                      'error 'warning))
                " "
                (propertize (format "%-4s" (alist-get 'min c)) 'face 'world-cup-meta)
                (format "%s (%s)" (or (alist-get 'player c) "?") (alist-get 'code c))
                "\n"))
      ;; team stats table
      (when stats
        (let ((hst (alist-get (intern h) stats)) (ast (alist-get (intern a) stats)))
          (insert "\n " (propertize (format "%-22s %8s   %-8s" "" h a)
                                    'face 'world-cup-column-header) "\n")
          (dolist (row '(("Possession" . "possessionPct") ("Shots" . "totalShots")
                         ("Shots on target" . "shotsOnTarget") ("Corners" . "wonCorners")
                         ("Fouls" . "foulsCommitted") ("Offsides" . "offsides")
                         ("Yellow cards" . "yellowCards") ("Red cards" . "redCards")
                         ("Saves" . "saves")))
            (let ((k (intern (cdr row))))
              (insert (format " %-22s %8s   %-8s\n"
                              (car row)
                              (or (alist-get k hst) "-")
                              (or (alist-get k ast) "-")))))))
      (insert "\n"))))

(defun world-cup-game--render ()
  "Render the game buffer from its buffer-local state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (world-cup-game--insert-stats)
    (world-cup-game--insert-result)
    (when-let ((note (world-cup-match-note world-cup-game--match)))
      (insert "\n")
      (let ((start (point)))
        (insert " " (propertize note 'face 'world-cup-note) "\n")
        (let ((fill-column 84) (left-margin 1) (fill-prefix " "))
          (fill-region start (point)))))
    (insert "\n" (propertize " Press ? for actions"
                             'face 'world-cup-meta) "\n")
    (goto-char (point-min))))

(defun world-cup--match-finished-p (match)
  "Return non-nil if MATCH has already been played (kickoff was over 2h ago)."
  (when-let ((time (world-cup--match-time match)))
    (time-less-p (time-add time (* 2 60 60)) (current-time))))

(defun world-cup-game-youtube-preview ()
  "Search YouTube for this game and stream the choice with mpv.
Searches for \"highlights\" if the game has already been played, else \"preview\"."
  (interactive)
  (world-cup-game--guard)
  (world-cup-youtube-watch
   (format "%s %s" (world-cup-game--teams)
           (if (world-cup--match-finished-p world-cup-game--match)
               "highlights" "preview"))))

(defun world-cup-game-web-xwidget ()
  "Run a web search for this game in an xwidget-webkit buffer."
  (interactive)
  (world-cup-game--guard)
  (unless (fboundp 'xwidget-webkit-browse-url)
    (user-error "This Emacs was not built with xwidget support"))
  (xwidget-webkit-browse-url (world-cup-game--web-url)))

(defun world-cup-game-web-browser ()
  "Run a web search for this game in the external browser."
  (interactive)
  (world-cup-game--guard)
  (browse-url (world-cup-game--web-url)))

(defun world-cup-game-jump-team-a ()
  "Open the team page for the home team of this game."
  (interactive)
  (world-cup-game--guard)
  (let ((team (world-cup--match-team world-cup-game--match 'a)))
    (if team (world-cup-display-team team)
      (user-error "No team page for %s" (alist-get 'team_a world-cup-game--match)))))

(defun world-cup-game-jump-team-b ()
  "Open the team page for the away team of this game."
  (interactive)
  (world-cup-game--guard)
  (let ((team (world-cup--match-team world-cup-game--match 'b)))
    (if team (world-cup-display-team team)
      (user-error "No team page for %s" (alist-get 'team_b world-cup-game--match)))))

(defun world-cup-game-reload ()
  "Re-render the current game buffer."
  (interactive)
  (world-cup-game--guard)
  (world-cup-game--render))

;;;###autoload
(defun world-cup-display-game (match)
  "Show a buffer for MATCH: basic stats plus a `?' actions menu."
  (let ((buf (get-buffer-create
              (format "*World Cup Game: %s vs %s (#%d)*"
                      (alist-get 'team_a match)
                      (alist-get 'team_b match)
                      (alist-get 'match_number match)))))
    (with-current-buffer buf
      (world-cup-game-mode)
      (setq world-cup-game--match match)
      (world-cup-game--render))
    (pop-to-buffer buf)))

;;;; Fixture search

(defun world-cup--fixture-candidates ()
  "Return an alist of (DISPLAY . MATCH) for every game."
  (mapcar (lambda (m)
            (cons (format "%3d. %s vs %s   %s %5s  %s"
                          (alist-get 'match_number m)
                          (world-cup--pad (alist-get 'team_a m) 22)
                          (world-cup--pad (alist-get 'team_b m) 22)
                          (world-cup--local-date m)
                          (world-cup--local-time m)
                          (world-cup--match-label m))
                  m))
          (world-cup-matches)))

(defun world-cup--annotate-fixture (cands)
  "Return an annotation function for the fixture candidate alist CANDS."
  (lambda (cand)
    (when-let* ((m (cdr (assoc cand cands))))
      (propertize (format "  %s, %s" (alist-get 'venue m) (alist-get 'city m))
                  'face 'world-cup-meta))))

;;;###autoload
(defun world-cup-consult-fixture ()
  "Search all World Cup games (by team) and open the selected game's page."
  (interactive)
  (let* ((cands (world-cup--fixture-candidates))
         (annotate (world-cup--annotate-fixture cands))
         (names (mapcar #'car cands))
         (choice
          (if (fboundp 'consult--read)
              (consult--read names
                             :prompt "World Cup game: "
                             :category 'world-cup-fixture
                             :require-match t
                             :sort nil
                             :annotate annotate)
            (let ((completion-extra-properties
                   (list :annotation-function annotate)))
              (completing-read "World Cup game: " names nil t))))
         (m (cdr (assoc choice cands))))
    (when m
      (world-cup-display-game m))))

;;;; Dashboard (group standings)

(defib world-cup-team-button ()
  "Hyperbole implicit button: a team name; opens that team's page.
The text carries a `world-cup-team-ref' property (the team alist) placed
by renderers such as the dashboard."
  (let ((team (get-text-property (point) 'world-cup-team-ref)))
    (when team
      (let* ((pos (point))
             (start (if (and (> pos (point-min))
                             (get-text-property (1- pos) 'world-cup-team-ref))
                        (previous-single-property-change pos 'world-cup-team-ref)
                      pos))
             (end (or (next-single-property-change pos 'world-cup-team-ref)
                      (point-max))))
        (ibut:label-set (world-cup-team-name team) start end)
        (hact 'world-cup-display-team team)))))

(defun world-cup--groups ()
  "Return an alist (GROUP-LETTER . (TEAM ...)) derived from the schedule.
Groups are sorted A..L; teams within a group are sorted by name."
  (let ((table (make-hash-table :test 'equal))
        (order nil))
    (dolist (m (world-cup-matches))
      (let ((g (alist-get 'group m))
            (a (alist-get 'team_a_code m))
            (b (alist-get 'team_b_code m)))
        (when (and g a b)
          (unless (member g order) (push g order))
          (dolist (code (list a b))
            (puthash g (cons code (gethash g table)) table)))))
    (mapcar
     (lambda (g)
       (cons g
             (sort (delq nil (mapcar #'world-cup--find-team-by-code
                                     (delete-dups (gethash g table))))
                   (lambda (x y) (string< (world-cup-team-name x)
                                          (world-cup-team-name y))))))
     (sort order #'string<))))

(defvar world-cup-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    map)
  "Keymap for `world-cup-dashboard-mode'.")

(define-derived-mode world-cup-dashboard-mode magit-section-mode "WC-Dashboard"
  "Major mode for the World Cup dashboard / group standings.")

(defun world-cup-dashboard--insert-team-row (rank team v)
  "Insert a standings row: RANK, TEAM (a button) and stat vector V.
Top two of a group are highlighted as qualifying."
  (let* ((gd (- (aref v 4) (aref v 5)))
         (name (propertize
                (world-cup--pad (format "%s (%s)"
                                        (world-cup-team-name team)
                                        (world-cup-team-code team))
                                26)
                'world-cup-team-ref team
                'face (if (<= rank 2) 'world-cup-position-fw 'world-cup-player-link)
                'help-echo "Action key: open team page")))
    (insert (propertize (format " %d. " rank)
                        'face (if (<= rank 2) 'world-cup-rank 'world-cup-meta))
            name
            (format " %3d %3d %3d %3d %3d %3d %+4d %4d\n"
                    (aref v 0) (aref v 1) (aref v 2) (aref v 3)
                    (aref v 4) (aref v 5) gd (aref v 6)))))

(defun world-cup-dashboard--insert-group (letter teams)
  "Insert a foldable standings table for group LETTER with TEAMS."
  (magit-insert-section (world-cup-group letter)
    (world-cup--heading (format "Group %s" letter))
    (insert "     "
            (propertize (format "%-26s %3s %3s %3s %3s %3s %3s %4s %4s"
                                "Team" "MP" "W" "D" "L" "GF" "GA" "GD" "Pts")
                        'face 'world-cup-column-header)
            "\n")
    (let ((rank 0))
      (dolist (entry (world-cup--group-standings teams))
        (world-cup-dashboard--insert-team-row (cl-incf rank) (car entry) (cdr entry))))
    (insert "\n")))

(defun world-cup--insert-leaderboard (title key entries)
  "Insert a foldable leaderboard section TITLE ranking ENTRIES by KEY (desc)."
  (magit-insert-section (world-cup-leaderboard title)
    (world-cup--heading title)
    (let ((rank 0) (prev nil))
      (dolist (s entries)
        (let ((n (alist-get key s)))
          (setq rank (if (eq prev n) rank (1+ rank)) prev n)
          (when (<= rank 15)
            (insert (propertize (format "  %2d  " n) 'face 'world-cup-rank)
                    (format "%-26s " (alist-get 'player s))
                    (propertize (format "%s" (alist-get 'code s))
                                'face 'world-cup-code)
                    "\n")))))
    (insert "\n")))

(defun world-cup-dashboard--insert-golden-boot ()
  "Insert a foldable Golden Boot leaderboard from `world-cup-scorers'."
  (when-let ((scorers (world-cup-scorers)))
    (world-cup--insert-leaderboard "Golden Boot" 'goals scorers)))

(defun world-cup-dashboard--insert-assists ()
  "Insert a foldable assists leaderboard from `world-cup-assist-leaders'."
  (when-let ((leaders (world-cup-assist-leaders)))
    (world-cup--insert-leaderboard "Assists" 'assists leaders)))

(defun world-cup-dashboard--render ()
  "Render the dashboard buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (world-cup-dashboard)
      (insert (propertize " FIFA World Cup 2026 \u2014 Group Standings"
                          'face 'world-cup-summary-title)
              "\n"
              (propertize " Press ? for actions" 'face 'world-cup-meta)
              "\n\n")
      (dolist (entry (world-cup--groups))
        (world-cup-dashboard--insert-group (car entry) (cdr entry)))
      (world-cup-dashboard--insert-golden-boot)
      (world-cup-dashboard--insert-assists))
    (goto-char (point-min))))

(defun world-cup-dashboard-revert ()
  "Reload data and re-render the dashboard."
  (interactive)
  (world-cup-load-data t)
  (when (derived-mode-p 'world-cup-dashboard-mode)
    (world-cup-dashboard--render)))

;;;###autoload
(defun world-cup-dashboard ()
  "Open the World Cup dashboard showing group standings.
Team names are buttons that open the corresponding team page."
  (interactive)
  (let ((buf (get-buffer-create "*World Cup Dashboard*")))
    (with-current-buffer buf
      (world-cup-dashboard-mode)
      (world-cup-dashboard--render))
    (pop-to-buffer buf)))

;;;; Live results (ESPN public endpoints)

(defconst world-cup--espn-base
  "https://site.api.espn.com/apis/site/v2/sports/soccer/fifa.world"
  "Base URL for ESPN's public (unofficial) men's World Cup endpoints.")

(defun world-cup--http-json (url)
  "GET URL and parse the JSON body into alists/lists, or nil on failure."
  (when-let ((body (world-cup--http-get url)))
    (ignore-errors
      (json-parse-string body :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil))))

(defun world-cup--pair-key (a b)
  "Return an order-independent key for team codes A and B."
  (mapconcat #'identity (sort (list a b) #'string<) "|"))

(defun world-cup--espn-summary (event-id)
  "Fetch and return the ESPN match summary for EVENT-ID, or nil."
  (world-cup--http-json
   (format "%s/summary?event=%s" world-cup--espn-base event-id)))

(defun world-cup--summary-team-stats (summary idcode)
  "Return alist CODE -> (statname . value) from SUMMARY's boxscore.
IDCODE maps ESPN team id -> our code."
  (let ((out nil))
    (dolist (tm (alist-get 'teams (alist-get 'boxscore summary)))
      (let ((code (gethash (alist-get 'id (alist-get 'team tm)) idcode))
            (a nil))
        (dolist (s (alist-get 'statistics tm))
          (push (cons (alist-get 'name s) (alist-get 'displayValue s)) a))
        (when code (push (cons (intern code) (nreverse a)) out))))
    out))

(defun world-cup--clock-minute (s)
  "Parse an ESPN clock like \"56'\" or \"90'+2'\" into an integer minute."
  (when (and s (string-match "\\`\\([0-9]+\\)\\(?:[^0-9]*\\+\\([0-9]+\\)\\)?" s))
    (+ (string-to-number (match-string 1 s))
       (if (match-string 2 s) (string-to-number (match-string 2 s)) 0))))

(defun world-cup--summary-players (summary players ctx)
  "Aggregate per-player appearances/minutes/goals/etc from SUMMARY into PLAYERS.
PLAYERS is a hash keyed by \"CODE-JERSEY\" of accumulating stat alists.
CTX is a plist (:num :home (CODE . SCORE) :away (CODE . SCORE)) used to record
a per-match line for each player."
  (let ((subin (make-hash-table :test 'equal))
        (subout (make-hash-table :test 'equal))
        (redmin (make-hash-table :test 'equal)))
    (dolist (x (alist-get 'keyEvents summary))
      (let ((typ (alist-get 'text (alist-get 'type x)))
            (mn (world-cup--clock-minute (alist-get 'displayValue (alist-get 'clock x))))
            (ps (alist-get 'participants x)))
        (cond
         ((equal typ "Substitution")
          (when (and ps mn)
            (when-let ((i (alist-get 'id (alist-get 'athlete (nth 0 ps))))) (puthash i mn subin))
            (when-let ((o (alist-get 'id (alist-get 'athlete (nth 1 ps))))) (puthash o mn subout))))
         ((and typ (string-prefix-p "Red" typ) ps mn)
          (when-let ((i (alist-get 'id (alist-get 'athlete (car ps))))) (puthash i mn redmin))))))
    (dolist (tm (alist-get 'rosters summary))
      (let ((code (upcase (or (alist-get 'abbreviation (alist-get 'team tm)) ""))))
        (dolist (p (alist-get 'roster tm))
          (let* ((aid (alist-get 'id (alist-get 'athlete p)))
                 (jersey (alist-get 'jersey p))
                 (name (alist-get 'displayName (alist-get 'athlete p)))
                 (starter (and (alist-get 'starter p) t))
                 (subbed-in (and (alist-get 'subbedIn p) t))
                 (played (or starter subbed-in))
                 (st (let (h) (dolist (s (alist-get 'stats p))
                                (push (cons (alist-get 'name s) (alist-get 'displayValue s)) h))
                          h))
                 (sval (lambda (k) (string-to-number (or (cdr (assoc k st)) "0"))))
                 (end (let ((e 90))
                        (when (gethash aid subout) (setq e (gethash aid subout)))
                        (when (and (gethash aid redmin) (< (gethash aid redmin) e))
                          (setq e (gethash aid redmin)))
                        e))
                 (start (or (gethash aid subin) 0))
                 (minutes (if played (max 0 (- end start)) 0))
                 (key (and jersey (intern (format "%s-%s" code jersey)))))
            (when (and played key (not (string-empty-p code)))
              (let ((cur (gethash key players)))
                (unless cur
                  (setq cur (list (cons 'player name) (cons 'code code)
                                  (cons 'jersey jersey) (cons 'apps 0) (cons 'starts 0)
                                  (cons 'subs 0) (cons 'minutes 0) (cons 'goals 0)
                                  (cons 'assists 0) (cons 'yellow 0) (cons 'red 0)
                                  (cons 'matches nil))))
                (cl-incf (alist-get 'apps cur))
                (when starter (cl-incf (alist-get 'starts cur)))
                (when subbed-in (cl-incf (alist-get 'subs cur)))
                (cl-incf (alist-get 'minutes cur) minutes)
                (cl-incf (alist-get 'goals cur) (funcall sval "totalGoals"))
                (cl-incf (alist-get 'assists cur) (funcall sval "goalAssists"))
                (cl-incf (alist-get 'yellow cur) (funcall sval "yellowCards"))
                (cl-incf (alist-get 'red cur) (funcall sval "redCards"))
                (let* ((home (plist-get ctx :home)) (away (plist-get ctx :away))
                       (mine (if (equal code (car home)) home away))
                       (their (if (equal code (car home)) away home))
                       (gf (cdr mine)) (ga (cdr their)))
                  (setf (alist-get 'matches cur)
                        (cons (list (cons 'num (plist-get ctx :num))
                                    (cons 'opp (car their))
                                    (cons 'gf gf) (cons 'ga ga)
                                    (cons 'res (cond ((> gf ga) "W") ((< gf ga) "L") (t "D")))
                                    (cons 'min minutes) (cons 'started starter)
                                    (cons 'g (funcall sval "totalGoals"))
                                    (cons 'a (funcall sval "goalAssists")))
                              (alist-get 'matches cur))))
                (puthash key cur players)))))))))

(defun world-cup--ingest-event (e pair-index matches scorers players)
  "Parse ESPN event E: fill MATCHES, SCORERS and PLAYERS.
PAIR-INDEX maps a `world-cup--pair-key' to our match number.  For played
games the summary is fetched once (team stats + per-player aggregation)."
  (let* ((comp (car (alist-get 'competitions e)))
         (status (alist-get 'status comp))
         (state (alist-get 'state (alist-get 'type status)))
         (detail (alist-get 'shortDetail (alist-get 'type status)))
         (idcode (make-hash-table :test 'equal))
         home away)
    (dolist (c (alist-get 'competitors comp))
      (let* ((team (alist-get 'team c))
             (code (upcase (or (alist-get 'abbreviation team) "")))
             (score (string-to-number (or (alist-get 'score c) "0"))))
        (puthash (alist-get 'id team) code idcode)
        (if (equal (alist-get 'homeAway c) "home")
            (setq home (cons code score))
          (setq away (cons code score)))))
    (when (and home away)
      (let (goals cards)
        (dolist (d (alist-get 'details comp))
          (let* ((code (gethash (alist-get 'id (alist-get 'team d)) idcode))
                 (ath (car (alist-get 'athletesInvolved d)))
                 (name (and ath (alist-get 'displayName ath)))
                 (aid (and ath (alist-get 'id ath)))
                 (mn (alist-get 'displayValue (alist-get 'clock d)))
                 (own (and (alist-get 'ownGoal d) t))
                 (pen (and (alist-get 'penaltyKick d) t)))
            (cond
             ((alist-get 'scoringPlay d)
              (push (list (cons 'min mn) (cons 'code code) (cons 'player name)
                          (cons 'pen pen) (cons 'own own))
                    goals)
              (when (and aid name (not own))
                (let ((cur (gethash aid scorers)))
                  (if cur
                      (setf (alist-get 'goals cur) (1+ (alist-get 'goals cur)))
                    (setq cur (list (cons 'player name) (cons 'code code)
                                    (cons 'goals 1))))
                  (puthash aid cur scorers))))
             ((or (alist-get 'redCard d) (alist-get 'yellowCard d))
              (push (list (cons 'min mn) (cons 'code code) (cons 'player name)
                          (cons 'color (if (alist-get 'redCard d) "red" "yellow")))
                    cards)))))
        (let ((summary (and (member state '("in" "post"))
                            (world-cup--espn-summary (alist-get 'id e))))
              (num (gethash (world-cup--pair-key (car home) (car away)) pair-index)))
          (when summary
            (world-cup--summary-players
             summary players (list :num num :home home :away away)))
          (when num
            (let ((res (list (cons 'status state) (cons 'detail detail)
                             (cons 'home (car home)) (cons 'away (car away))
                             (cons 'home_score (cdr home))
                             (cons 'away_score (cdr away))
                             (cons 'goals (nreverse goals))
                             (cons 'cards (nreverse cards)))))
              (when summary
                (when-let ((st (world-cup--summary-team-stats summary idcode)))
                  (setq res (append res (list (cons 'stats st))))))
              (puthash (number-to-string num) res matches))))))))

;;;###autoload
(defun world-cup-refresh-results ()
  "Fetch results for games played so far from ESPN; write `world-cup-results-file'.
Updates scores, possession/shots, goals and cards per match, and a global
golden-boot tally, then reloads the data."
  (interactive)
  (world-cup-load-data)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (pair-index (make-hash-table :test 'equal))
         (matches (make-hash-table :test 'equal))
         (scorers (make-hash-table :test 'equal))
         (players (make-hash-table :test 'equal))
         (dates (sort (seq-filter (lambda (d) (and d (not (string-lessp today d))))
                                  (delete-dups
                                   (mapcar (lambda (m) (alist-get 'date m))
                                           (world-cup-matches))))
                      #'string<)))
    (dolist (m (world-cup-matches))
      (let ((a (alist-get 'team_a_code m)) (b (alist-get 'team_b_code m)))
        (when (and a b)
          (puthash (world-cup--pair-key a b) (alist-get 'match_number m) pair-index))))
    (dolist (date dates)
      (message "World Cup: fetching %s\u2026" date)
      (let ((sb (world-cup--http-json
                 (format "%s/scoreboard?dates=%s" world-cup--espn-base
                         (replace-regexp-in-string "-" "" date)))))
        (dolist (e (alist-get 'events sb))
          (world-cup--ingest-event e pair-index matches scorers players))))
    (let ((slist nil) (malist nil) (palist nil))
      (maphash (lambda (_ v) (push v slist)) scorers)
      (setq slist (sort slist (lambda (x y) (> (alist-get 'goals x)
                                               (alist-get 'goals y)))))
      (maphash (lambda (k v) (push (cons (intern k) v) malist)) matches)
      (maphash (lambda (k v) (push (cons k v) palist)) players)
      (with-temp-file (world-cup--path world-cup-results-file)
        (insert (json-encode
                 (list (cons 'updated (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                       (cons 'matches malist)
                       (cons 'scorers slist)
                       (cons 'players palist)))))
      (world-cup-load-data t)
      (message "World Cup: updated %d matches, %d scorers, %d players"
               (hash-table-count matches) (length slist)
               (hash-table-count players)))))

;;;; Standings + golden boot (computed from results)

(defun world-cup--group-standings (teams)
  "Return TEAMS as a list of (TEAM . STATS) sorted by points, GD, GF.
STATS is a vector [gp w d l gf ga pts]; only finished (post) games count."
  (let ((stat (make-hash-table :test 'equal))
        (codes (mapcar #'world-cup-team-code teams)))
    (dolist (c codes) (puthash c (vector 0 0 0 0 0 0 0) stat))
    (cl-flet ((acc (v gf ga)
                (aset v 0 (1+ (aref v 0)))
                (aset v 4 (+ (aref v 4) gf))
                (aset v 5 (+ (aref v 5) ga))
                (cond ((> gf ga) (aset v 1 (1+ (aref v 1))) (aset v 6 (+ (aref v 6) 3)))
                      ((= gf ga) (aset v 2 (1+ (aref v 2))) (aset v 6 (+ (aref v 6) 1)))
                      (t (aset v 3 (1+ (aref v 3)))))))
      (dolist (m (world-cup-matches))
        (let ((a (alist-get 'team_a_code m)) (b (alist-get 'team_b_code m)))
          (when (and (member a codes) (member b codes))
            (let ((r (world-cup-match-result m)))
              (when (and r (equal (alist-get 'status r) "post"))
                (let ((hc (alist-get 'home r)) (ac (alist-get 'away r))
                      (hs (alist-get 'home_score r)) (as (alist-get 'away_score r)))
                  (acc (gethash hc stat) hs as)
                  (acc (gethash ac stat) as hs))))))))
    (sort (mapcar (lambda (tm) (cons tm (gethash (world-cup-team-code tm) stat))) teams)
          (lambda (x y)
            (let ((vx (cdr x)) (vy (cdr y)))
              (cond ((/= (aref vx 6) (aref vy 6)) (> (aref vx 6) (aref vy 6)))
                    ((/= (- (aref vx 4) (aref vx 5)) (- (aref vy 4) (aref vy 5)))
                     (> (- (aref vx 4) (aref vx 5)) (- (aref vy 4) (aref vy 5))))
                    (t (> (aref vx 4) (aref vy 4)))))))))

;;;; Transient menus (press ? in each buffer)

(transient-define-prefix world-cup-player-menu ()
  "Actions for a World Cup player buffer."
  [["Search"
    ("y" "YouTube highlights"     world-cup-player-youtube-highlights)
    ("x" "Web search (xwidget)"   world-cup-player-web-xwidget)
    ("b" "Web search (browser)"   world-cup-player-web-browser)]
   ["Article"
    ("TAB" "Toggle full article"  world-cup-player-toggle-detail)
    ("g"   "Reload"               world-cup-player-reload)]
   ["Buffer"
    ("q" "Quit" quit-window)]])

(transient-define-prefix world-cup-team-menu ()
  "Actions for a World Cup team buffer."
  [["Browse"
    ("t" "Switch team\u2026"      world-cup-consult-team)
    ("p" "Find player\u2026"      world-cup-consult-player)
    ("f" "Find game\u2026"        world-cup-consult-fixture)
    ("g" "Reload"                world-cup-team-revert)]
   ["Buffer"
    ("q" "Quit" quit-window)]])

(transient-define-prefix world-cup-dashboard-menu ()
  "Actions for the World Cup dashboard."
  [["Browse"
    ("t" "Find team\u2026"   world-cup-consult-team)
    ("p" "Find player\u2026" world-cup-consult-player)
    ("f" "Find game\u2026"   world-cup-consult-fixture)]
   ["Data"
    ("u" "Refresh results (ESPN)" world-cup-refresh-results)
    ("g" "Reload"           world-cup-dashboard-revert)]
   ["Buffer"
    ("q" "Quit" quit-window)]])

(transient-define-prefix world-cup-game-menu ()
  "Actions for a World Cup game buffer."
  [["Search"
    ("y" "YouTube preview"       world-cup-game-youtube-preview)
    ("x" "Web search (xwidget)"  world-cup-game-web-xwidget)
    ("b" "Web search (browser)"  world-cup-game-web-browser)]
   ["Teams"
    ("1" world-cup-game-jump-team-a
     :description (lambda () (format "Page: %s"
                                    (alist-get 'team_a world-cup-game--match))))
    ("2" world-cup-game-jump-team-b
     :description (lambda () (format "Page: %s"
                                    (alist-get 'team_b world-cup-game--match))))]
   ["Buffer"
    ("f" "Find game\u2026" world-cup-consult-fixture)
    ("g" "Reload" world-cup-game-reload)
    ("q" "Quit" quit-window)]])

(transient-define-prefix world-cup-summary-menu ()
  "Actions for a Wikipedia summary buffer."
  [["Article"
    ("TAB" "Toggle full article" world-cup-summary-toggle-detail)
    ("RET" "Open in browser"     world-cup-summary-browse-url)]
   ["Buffer"
    ("q" "Quit" quit-window)]])

;;;; Keybindings (plain + evil)

(defconst world-cup--player-keys
  '(("y" . world-cup-player-youtube-highlights)
    ("x" . world-cup-player-web-xwidget)
    ("b" . world-cup-player-web-browser)
    ("TAB" . world-cup-player-toggle-detail)
    ("g" . world-cup-player-reload)
    ("?" . world-cup-player-menu)
    ("q" . quit-window))
  "Key bindings for `world-cup-player-mode'.")

(defconst world-cup--team-keys
  '(("g" . world-cup-team-revert)
    ("t" . world-cup-consult-team)
    ("p" . world-cup-consult-player)
    ("f" . world-cup-consult-fixture)
    ("?" . world-cup-team-menu)
    ("q" . quit-window))
  "Extra key bindings for `world-cup-team-mode'.")

(defconst world-cup--game-keys
  '(("y" . world-cup-game-youtube-preview)
    ("x" . world-cup-game-web-xwidget)
    ("b" . world-cup-game-web-browser)
    ("1" . world-cup-game-jump-team-a)
    ("2" . world-cup-game-jump-team-b)
    ("f" . world-cup-consult-fixture)
    ("g" . world-cup-game-reload)
    ("?" . world-cup-game-menu)
    ("q" . quit-window))
  "Key bindings for `world-cup-game-mode'.")

(defconst world-cup--summary-keys
  '(("TAB" . world-cup-summary-toggle-detail)
    ("+" . world-cup-summary-toggle-detail)
    ("RET" . world-cup-summary-browse-url)
    ("?" . world-cup-summary-menu)
    ("q" . quit-window))
  "Key bindings for `world-cup-summary-mode'.")

(defconst world-cup--dashboard-keys
  '(("g" . world-cup-dashboard-revert)
    ("u" . world-cup-refresh-results)
    ("t" . world-cup-consult-team)
    ("p" . world-cup-consult-player)
    ("f" . world-cup-consult-fixture)
    ("?" . world-cup-dashboard-menu)
    ("q" . quit-window))
  "Key bindings for `world-cup-dashboard-mode'.")

(defun world-cup--apply-keys (map bindings)
  "Bind BINDINGS (alist of KEY . COMMAND) into MAP with `define-key'."
  (dolist (b bindings)
    (define-key map (kbd (car b)) (cdr b))))

(defun world-cup--evil-bind (map bindings)
  "Bind BINDINGS in MAP for evil normal and motion states."
  (when (fboundp 'evil-define-key*)
    (apply #'evil-define-key* '(normal motion) map
           (mapcan (lambda (b) (list (kbd (car b)) (cdr b))) bindings))))

;; Apply on every load so re-evaluating the file picks up binding changes
;; (a `defvar' keymap initializer would not re-run).
(world-cup--apply-keys world-cup-player-mode-map world-cup--player-keys)
(world-cup--apply-keys world-cup-team-mode-map world-cup--team-keys)
(world-cup--apply-keys world-cup-summary-mode-map world-cup--summary-keys)
(world-cup--apply-keys world-cup-game-mode-map world-cup--game-keys)
(world-cup--apply-keys world-cup-dashboard-mode-map world-cup--dashboard-keys)

(with-eval-after-load 'evil
  (world-cup--evil-bind world-cup-player-mode-map world-cup--player-keys)
  (world-cup--evil-bind world-cup-team-mode-map world-cup--team-keys)
  (world-cup--evil-bind world-cup-summary-mode-map world-cup--summary-keys)
  (world-cup--evil-bind world-cup-game-mode-map world-cup--game-keys)
  (world-cup--evil-bind world-cup-dashboard-mode-map world-cup--dashboard-keys))

(provide 'world-cup)

;;; world-cup.el ends here
