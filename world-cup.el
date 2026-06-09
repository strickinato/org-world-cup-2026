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

;;;; Faces

(defface world-cup-position-gk '((t :inherit font-lock-builtin-face))
  "Face for goalkeepers." :group 'world-cup)
(defface world-cup-position-df '((t :inherit font-lock-keyword-face))
  "Face for defenders." :group 'world-cup)
(defface world-cup-position-mf '((t :inherit font-lock-function-name-face))
  "Face for midfielders." :group 'world-cup)
(defface world-cup-position-fw '((t :inherit font-lock-string-face))
  "Face for forwards." :group 'world-cup)
(defface world-cup-player-link '((t :inherit link))
  "Face for a clickable player name (Hyperbole implicit button)."
  :group 'world-cup)
(defface world-cup-summary-title '((t :inherit info-title-3))
  "Face for the title in a Wikipedia summary overlay." :group 'world-cup)


;;;; Data loading

(defvar world-cup--teams nil "Cached list of team alists.")
(defvar world-cup--matches nil "Cached list of match alists.")

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
                   'face 'font-lock-keyword-face)
       (propertize (format "  %d players" (length (world-cup-team-players team)))
                   'face 'font-lock-comment-face)
       (when-let ((coach (world-cup-team-coach-name team)))
         (propertize (format "  coach: %s" coach)
                     'face 'font-lock-comment-face))))))

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
      (insert " " (propertize desc 'face 'font-lock-comment-face) "\n\n"))
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
    (define-key map (kbd "RET") #'world-cup-summary-browse-url)
    (define-key map (kbd "TAB") #'world-cup-summary-toggle-detail)
    (define-key map (kbd "+")   #'world-cup-summary-toggle-detail)
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
                        'face 'font-lock-comment-face)
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
Activating it (action key / mouse) looks the player up on Wikipedia.
The name text carries a `world-cup-player-name' text property placed
by the roster renderer."
  (when (derived-mode-p 'world-cup-team-mode)
    (let ((name (get-text-property (point) 'world-cup-player-name)))
      (when name
        (let* ((pos (point))
               (start (if (and (> pos (point-min))
                               (get-text-property (1- pos) 'world-cup-player-name))
                          (previous-single-property-change
                           pos 'world-cup-player-name)
                        pos))
               (end (or (next-single-property-change
                         pos 'world-cup-player-name)
                        (point-max))))
          (ibut:label-set name start end)
          (hact 'world-cup-wikipedia-lookup name))))))

;;;###autoload
(defun world-cup-wikipedia-lookup (&optional query)
  "Search Wikipedia for QUERY via consult and show a summary with image.
When called from a Hyperbole button, QUERY is the button label (player name).
Interactively, prompt for the search string.  The chosen article's summary
(with image) is shown in a dedicated buffer."
  (interactive)
  (let* ((query (or query (read-string "Wikipedia search: ")))
         (results (world-cup--wikipedia-search query)))
    (unless results
      (user-error "No Wikipedia results for %s" query))
    (let* ((cands (mapcar (lambda (r) (cons (plist-get r :title) r)) results))
           (titles (mapcar #'car cands))
           (annotate
            (lambda (cand)
              (when-let* ((r (cdr (assoc cand cands)))
                          (d (plist-get r :desc))
                          ((not (string-empty-p d))))
                (concat "  " (propertize d 'face 'font-lock-comment-face)))))
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
                (completing-read prompt titles nil t))))
           (r (cdr (assoc choice cands))))
      (when r
        (world-cup--show-summary
         (world-cup--wikipedia-summary (plist-get r :title)))))))

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
                                        'face 'font-lock-comment-face))
                   (when (numberp views)
                     (propertize (format "  %s views" views)
                                 'face 'font-lock-comment-face)))))))
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
                        'world-cup-player-name name
                        'face 'world-cup-player-link
                        'help-echo "Action key: look this player up on Wikipedia"))
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
                    'face 'magit-section-heading))
      (insert "  "
              (propertize
               (format "%-38s%-4s%-34s%-5s%-7s%s\n"
                       "Name" "Pos" "Club (Nat)" "Age" "Ht" "#")
               'face 'font-lock-comment-face))
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
Activating it searches YouTube for a match preview and streams the
chosen video with mpv.  The row carries a `world-cup-fixture' text
property (the search query) placed by the fixtures renderer."
  (when (derived-mode-p 'world-cup-team-mode)
    (let ((query (get-text-property (point) 'world-cup-fixture)))
      (when query
        (let* ((pos (point))
               (start (if (and (> pos (point-min))
                               (get-text-property (1- pos) 'world-cup-fixture))
                          (previous-single-property-change
                           pos 'world-cup-fixture)
                        pos))
               (end (or (next-single-property-change
                         pos 'world-cup-fixture)
                        (point-max))))
          (ibut:label-set query start end)
          (hact 'world-cup-youtube-watch query))))))

(defun world-cup--insert-fixture (team match)
  "Insert one MATCH row for TEAM inside a `magit-section'.
The row is a `world-cup-fixture' Hyperbole implicit button."
  (pcase-let* ((`(,opp . ,home-p) (world-cup--opponent team match))
               (grp (alist-get 'group match))
               (label (if grp (format "Grp %s" grp)
                        (or (alist-get 'stage match) "")))
               (query (format "%s vs %s preview"
                              (world-cup-team-name team) opp))
               (line (format "  %s %5s  %-7s  %s %-24s  %s"
                             (alist-get 'date match)
                             (alist-get 'time_et match)
                             label
                             (if home-p "vs" "@ ")
                             (propertize (world-cup--pad opp 24)
                                         'face 'world-cup-player-link)
                             (propertize
                              (format "%s, %s"
                                      (alist-get 'venue match)
                                      (alist-get 'city match))
                              'face 'font-lock-comment-face))))
    (magit-insert-section (world-cup-match match)
      (magit-insert-heading
        (propertize line
                    'world-cup-fixture query
                    'help-echo (format "Action key: YouTube preview \u2014 %s"
                                       query))))))

(defun world-cup--insert-fixtures (team)
  "Insert the Fixtures section for TEAM."
  (let ((matches (world-cup-team-matches team)))
    (magit-insert-section (world-cup-fixtures)
      (magit-insert-heading
        (propertize (format "Fixtures (%d)" (length matches))
                    'face 'magit-section-heading))
      (if (null matches)
          (insert (propertize "  No scheduled matches found.\n"
                              'face 'font-lock-comment-face))
        (insert (propertize
                 (format "  %-10s %5s  %-7s  %-27s  %s\n"
                         "Date" "ET" "Stage" "Opponent" "Venue")
                 'face 'font-lock-comment-face))
        (dolist (m matches)
          (world-cup--insert-fixture team m)))
      (insert "\n"))))

;;;; Major mode + buffer

(defvar-local world-cup-team nil
  "The team alist displayed in the current `world-cup-team-mode' buffer.")

(defvar world-cup-team-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'world-cup-team-revert)
    (define-key map (kbd "t") #'world-cup-consult-team)
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
          (format " %s  [%s]   Coach: %s"
                  (world-cup-team-name team)
                  (world-cup-team-code team)
                  (or (world-cup-team-coach-name team) "?")))
    (magit-insert-section (world-cup-team-root)
      (world-cup--insert-fixtures team)
      (world-cup--insert-roster team))
    (goto-char (point-min))))

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

(provide 'world-cup)

;;; world-cup.el ends here
