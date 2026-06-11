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

(defcustom world-cup-summaries-file "world-cup-2026-team-summaries.json"
  "Name of the one-sentence team summaries JSON inside `world-cup-data-directory'."
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
  '((t :inherit magit-section-heading :underline t))
  "Face for section headings." :group 'world-cup-faces)

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
(defvar world-cup--summaries nil "Cached alist of CODE -> one-sentence summary.")
(defvar world-cup--analysis nil "Cached alist of CODE -> analysis alist.")
(defvar world-cup--fixture-notes nil "Cached alist of match-number -> note string.")
(defvar world-cup--fox-rankings nil "Cached alist of CODE -> (NUMBER -> ranking alist).")

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
  (when (or force (null world-cup--summaries))
    (let ((path (world-cup--path world-cup-summaries-file)))
      (setq world-cup--summaries
            (when (file-readable-p path)
              (alist-get 'summaries (world-cup--read-json path))))))
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

(defun world-cup-team-summary (team)
  "Return the one-sentence summary for TEAM, or nil."
  (world-cup-load-data)
  (when-let ((code (world-cup-team-code team)))
    (alist-get (intern code) world-cup--summaries)))

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
      (magit-insert-heading
        (propertize (format "Squad (%d players)" (length players))
                    'face 'world-cup-heading))
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
        (propertize line
                    'world-cup-fixture match
                    'help-echo "Action key: open game page"))
      (when-let ((note (world-cup-match-note match)))
        (let ((start (point)))
          (insert "       \u21b3 " (propertize note 'face 'world-cup-note) "\n")
          (let ((fill-column 84) (left-margin 9) (fill-prefix "         "))
            (fill-region start (point))))))))

(defun world-cup--insert-fixtures (team)
  "Insert the Fixtures section for TEAM."
  (let ((matches (world-cup-team-matches team)))
    (magit-insert-section (world-cup-fixtures)
      (magit-insert-heading
        (propertize (format "Fixtures (%d)" (length matches))
                    'face 'world-cup-heading))
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
      (when-let ((summary (world-cup-team-summary team)))
        (insert (propertize "\u201c" 'face 'world-cup-meta))
        (let ((start (point)))
          (insert (propertize summary 'face 'world-cup-quote))
          (insert (propertize "\u201d" 'face 'world-cup-meta))
          (let ((fill-column 78) (left-margin 1))
            (fill-region start (point))))
        (insert "\n\n"))
      (world-cup--insert-analysis team)
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

(defun world-cup--insert-analysis (team)
  "Insert the foldable Analysis section for TEAM, if available."
  (when-let ((a (world-cup-team-analysis team)))
    (magit-insert-section (world-cup-analysis)
      (magit-insert-heading
        (propertize "Analysis" 'face 'world-cup-heading))
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

(defun world-cup-player--render ()
  "Render the player buffer from its buffer-local state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (world-cup-player--insert-stats)
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

(defun world-cup-game--render ()
  "Render the game buffer from its buffer-local state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (world-cup-game--insert-stats)
    (when-let ((note (world-cup-match-note world-cup-game--match)))
      (insert "\n")
      (let ((start (point)))
        (insert " " (propertize note 'face 'world-cup-note) "\n")
        (let ((fill-column 84) (left-margin 1) (fill-prefix " "))
          (fill-region start (point)))))
    (insert "\n" (propertize " Press ? for actions"
                             'face 'world-cup-meta) "\n")
    (goto-char (point-min))))

(defun world-cup-game-youtube-preview ()
  "Search YouTube for a preview of this game and stream the choice with mpv."
  (interactive)
  (world-cup-game--guard)
  (world-cup-youtube-watch (format "%s preview" (world-cup-game--teams))))

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

(defun world-cup-dashboard--insert-team-row (team)
  "Insert a standings row for TEAM (a team-name button + zeroed stats)."
  (insert "  ")
  (insert (propertize
           (world-cup--pad (format "%s (%s)"
                                   (world-cup-team-name team)
                                   (world-cup-team-code team))
                           28)
           'world-cup-team-ref team
           'face 'world-cup-player-link
           'help-echo "Action key: open team page"))
  ;; No games played yet, so every figure is zero.
  (insert (format " %3d %3d %3d %3d %3d %3d %4d %4d\n" 0 0 0 0 0 0 0 0)))

(defun world-cup-dashboard--insert-group (letter teams)
  "Insert a foldable standings table for group LETTER with TEAMS."
  (magit-insert-section (world-cup-group letter)
    (magit-insert-heading
      (propertize (format "Group %s" letter) 'face 'world-cup-heading))
    (insert "  "
            (propertize (format "%-28s %3s %3s %3s %3s %3s %3s %4s %4s"
                                "Team" "MP" "W" "D" "L" "GF" "GA" "GD" "Pts")
                        'face 'world-cup-column-header)
            "\n")
    (dolist (team teams)
      (world-cup-dashboard--insert-team-row team))
    (insert "\n")))

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
        (world-cup-dashboard--insert-group (car entry) (cdr entry))))
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
    ("f" "Find game\u2026"   world-cup-consult-fixture)
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
