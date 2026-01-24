;;; flash-emacs.el --- Simple flash.nvim-like jump navigation for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Flash-Emacs Contributors
;; Maintainer: Flash-Emacs Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5"))
;; Keywords: navigation, jump, search, convenience
;; URL: https://github.com/flash-emacs/flash-emacs

;;; Commentary:

;; Flash-emacs provides flash.nvim-like jump navigation for Emacs.
;; This is a simplified version that only searches in visible windows.

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup flash-emacs nil
  "Fast navigation with labeled jumps."
  :group 'convenience
  :prefix "flash-emacs-")

(defcustom flash-emacs-labels "asdghklqwertyuiopzxcvbnmfj"
  "Characters used as jump labels."
  :type 'string
  :group 'flash-emacs)

(defcustom flash-emacs-multi-window t
  "Whether to search in all visible windows."
  :type 'boolean
  :group 'flash-emacs)

(defcustom flash-emacs-case-sensitive 'smart
  "How to handle case sensitivity in search.
- nil: always case-insensitive
- t: always case-sensitive
- 'smart: case-insensitive if pattern is all lowercase, case-sensitive if it contains uppercase"
  :type '(choice (const :tag "Always case-insensitive" nil)
          (const :tag "Always case-sensitive" t)
          (const :tag "Smart case (default)" smart))
  :group 'flash-emacs)

(defcustom flash-emacs-min-pattern-length 1
  "Minimum pattern length before showing labels."
  :type 'integer
  :group 'flash-emacs)

(defcustom flash-emacs-auto-jump-single nil
  "If non-nil, automatically jump when there is exactly one match."
  :type 'boolean
  :group 'flash-emacs)

(defcustom flash-emacs-exclude-modes
  '(image-mode
    pdf-view-mode
    doc-view-mode
    archive-mode
    tar-mode
    hexl-mode
    binary-mode
    ;; fundamental-mode  ; often used for binary files
    )
  "List of major modes to exclude from search.
These modes typically contain binary data or non-textual content."
  :type '(repeat symbol)
  :group 'flash-emacs)

(defcustom flash-emacs-exclude-functions
  '(flash-emacs--buffer-binary-p
    flash-emacs--buffer-too-large-p)
  "List of functions to determine if a buffer should be excluded.
Each function should take a buffer as argument and return non-nil to exclude it."
  :type '(repeat function)
  :group 'flash-emacs)

(defcustom flash-emacs-max-buffer-size 1048576  ; 1MB
  "Maximum buffer size to search in bytes.
Buffers larger than this are excluded to avoid performance issues."
  :type 'integer
  :group 'flash-emacs)

(defcustom flash-emacs-label-reuse 'lowercase
  "How to reuse labels for positions during search refinement.
- 'none: never reuse labels, assign new ones each time
- 'lowercase: reuse only lowercase labels (default)  
- 'all: reuse both lowercase and uppercase labels
This helps maintain label stability as you type more characters."
  :type '(choice (const :tag "Never reuse labels" none)
          (const :tag "Reuse lowercase labels" lowercase)
          (const :tag "Reuse all labels" all))
  :group 'flash-emacs)

;;; Faces

(defface flash-emacs-label
  '((t (:background "red" :foreground "white" :weight bold)))
  "Face for jump labels."
  :group 'flash-emacs)

(defface flash-emacs-match
  '((t (:background "yellow" :foreground "black")))
  "Face for search matches."
  :group 'flash-emacs)

;;; Internal variables

(defvar flash-emacs--overlays nil
  "List of active overlays.")

(defvar flash-emacs--label-positions nil
  "Hash table mapping position keys to previously assigned labels.
Used to maintain label stability across search refinements.")

(defvar flash-emacs--current-pattern nil
  "Current search pattern for tracking pattern changes.")

;;; Buffer exclusion functions

(defun flash-emacs--buffer-binary-p (buffer)
  "Return non-nil if BUFFER contains binary data.
This checks for null bytes and other indicators of binary content."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((sample-size (min 8192 (buffer-size))))  ; Check first 8KB
        (when (> sample-size 0)
          (or 
           ;; Check for null bytes (common in binary files)
           (search-forward "\0" (+ (point-min) sample-size) t)
           ;; Check for very high ratio of non-printable characters
           (let ((non-printable-count 0)
                 (char-count 0))
             (while (and (< (point) (+ (point-min) sample-size))
                         (not (eobp)))
               (let ((char (char-after)))
                 (when char
                   (setq char-count (1+ char-count))
                   (when (and (< char 32) (not (memq char '(?\t ?\n ?\r))))
                     (setq non-printable-count (1+ non-printable-count))))
                 (forward-char 1)))
             ;; If more than 30% non-printable chars, likely binary
             (and (> char-count 0)
                  (> (/ (* non-printable-count 100) char-count) 30)))))))))

(defun flash-emacs--buffer-too-large-p (buffer)
  "Return non-nil if BUFFER is too large for efficient searching."
  (with-current-buffer buffer
    (> (buffer-size) flash-emacs-max-buffer-size)))

(defun flash-emacs--buffer-excluded-p (buffer)
  "Return non-nil if BUFFER should be excluded from search.
Checks both major mode exclusions and custom exclusion functions."
  (with-current-buffer buffer
    (or 
     ;; Check if major mode is in exclusion list
     (memq major-mode flash-emacs-exclude-modes)
     ;; Check custom exclusion functions
     (cl-some (lambda (func)
                (and (functionp func)
                     (funcall func buffer)))
              flash-emacs-exclude-functions)
     ;; Skip special buffers (names starting with space) ONLY if they're empty or very small
     (let ((name (buffer-name)))
       (and (string-prefix-p " " name)
            (< (buffer-size) 10)))  ; Allow temp buffers with substantial content
     ;; Skip system/special buffers starting with * (except common useful ones)
     (let ((name (buffer-name)))
       (and (string-prefix-p "*" name)
            (not (string= "*scratch*" name))
            (not (string-match "\\*.*\\*<[0-9]+>$" name))  ; Allow numbered buffer copies
            ;; Don't exclude if it has a file or is a useful buffer
            (not (buffer-file-name))
            ;; Exclude common system buffers
            (or (string-match "^\\*\\(Messages\\|Completions\\|Help\\|Warnings\\|Backtrace\\|Compile-Log\\)" name)
                ;; Allow other * buffers that might have useful content
                nil))))))

;;; Search functions

(defun flash-emacs--should-ignore-case (pattern)
  "Determine if search should ignore case based on PATTERN and settings.
Returns t if case should be ignored (case-insensitive search)."
  (cond
   ;; Always case-insensitive
   ((eq flash-emacs-case-sensitive nil) t)
   ;; Always case-sensitive
   ((eq flash-emacs-case-sensitive t) nil)
   ;; Smart case: ignore case if pattern is all lowercase
   ((eq flash-emacs-case-sensitive 'smart)
    (string= pattern (downcase pattern)))
   ;; Default fallback
   (t t)))

(defun flash-emacs--get-window-bounds (window)
  "Get the visible line bounds for WINDOW.
Returns (start-line . end-line) where lines are 1-indexed."
  (let ((start-pos (window-start window))
        (end-pos (window-end window)))
    (with-current-buffer (window-buffer window)
      (save-excursion
        (goto-char start-pos)
        (let ((start-line (line-number-at-pos)))
          (goto-char end-pos)
          (let ((end-line (line-number-at-pos)))
            (cons start-line end-line)))))))

(defun flash-emacs--search-in-window (pattern window)
  "Search for PATTERN in WINDOW and return list of matches.
Only searches within the visible area of the window.
Skips buffers that should be excluded (binary, too large, etc.)."
  (let ((buffer (window-buffer window)))
    ;; Check if buffer should be excluded
    (if (flash-emacs--buffer-excluded-p buffer)
        '()  ; Return empty list for excluded buffers
      (let ((matches '())
            (case-fold-search (flash-emacs--should-ignore-case pattern)))
        (with-current-buffer buffer
          (save-excursion
            ;; Get visible window bounds
            (let* ((bounds (flash-emacs--get-window-bounds window))
                   (start-line (car bounds))
                   (end-line (cdr bounds)))
              ;; Move to start of visible area
              (goto-line start-line)
              (let ((search-start (point)))
                ;; Move to end of visible area
                (goto-line (1+ end-line))
                (let ((search-end (point)))
                  ;; Search only within visible bounds
                  (goto-char search-start)
                  (while (search-forward pattern search-end t)
                    (let ((match-start (match-beginning 0))
                          (match-end (match-end 0)))
                      (push (list :pos match-start
                                  :end-pos match-end
                                  :window window
                                  :buffer (current-buffer)
                                  :text (buffer-substring-no-properties match-start match-end))
                            matches))))))))
        (nreverse matches)))))

(defun flash-emacs--search-pattern (pattern)
  "Find all matches for PATTERN in currently visible windows only.
Automatically excludes windows with binary/image buffers and other unsuitable content."
  (when (>= (length pattern) flash-emacs-min-pattern-length)
    (let ((matches '())
          (windows (if flash-emacs-multi-window
                       (window-list)
                     (list (selected-window)))))

      ;; Search only in visible windows with suitable buffers
      (dolist (window windows)
        (when (and (window-live-p window)
                   (not (flash-emacs--buffer-excluded-p (window-buffer window))))
          (setq matches (append matches 
                                (flash-emacs--search-in-window pattern window)))))

      matches)))

;;; Label assignment

(defun flash-emacs--create-skip-pattern (search-pattern)
  "Create a skip pattern to avoid label conflicts with search continuation.
This pattern matches the search pattern followed by any character."
  (when (and search-pattern (> (length search-pattern) 0))
    (concat (regexp-quote search-pattern) ".")))

(defun flash-emacs--find-conflicting-labels (search-pattern labels window)
  "Find labels that would conflict with continuing the search pattern.
Returns a list of labels to exclude. Only searches within visible window bounds.
Skips excluded buffers (binary, image, etc.)."
  (let ((buffer (window-buffer window)))
    ;; Check if buffer should be excluded
    (if (flash-emacs--buffer-excluded-p buffer)
        '()  ; Return empty list for excluded buffers
      (let ((skip-pattern (flash-emacs--create-skip-pattern search-pattern))
            (conflicting-labels '())
            (case-fold-search (flash-emacs--should-ignore-case search-pattern)))
        (when skip-pattern
          (with-current-buffer buffer
            (save-excursion
              ;; Get visible window bounds
              (let* ((bounds (flash-emacs--get-window-bounds window))
                     (start-line (car bounds))
                     (end-line (cdr bounds)))
                ;; Move to start of visible area
                (goto-line start-line)
                (let ((search-start (point)))
                  ;; Move to end of visible area
                  (goto-line (1+ end-line))
                  (let ((search-end (point)))
                    ;; Search only within visible bounds
                    (goto-char search-start)
                    (while (re-search-forward skip-pattern search-end t)
                      (let* ((match-end (match-end 0))
                             (following-char (buffer-substring-no-properties 
                                              (1- match-end) match-end))
                             (matched-label
                              (and following-char
                                   (cl-find-if (lambda (label)
                                                 (if case-fold-search
                                                     (string= (downcase following-char)
                                                              (downcase label))
                                                   (string= following-char label)))
                                               labels))))
                        ;; Check if this following character is in our labels (case-aware)
                        (when matched-label
                          (push matched-label conflicting-labels))))))))))
        (delete-dups conflicting-labels)))))

(defun flash-emacs--filter-labels-for-pattern (labels search-pattern windows)
  "Filter out labels that would conflict with search pattern continuation.
Only checks windows with suitable (non-excluded) buffers."
  (if (or (not search-pattern) (= (length search-pattern) 0))
      labels
    (let ((conflicting-labels '()))
      ;; Collect conflicting labels from all suitable windows
      (dolist (window windows)
        (when (and (window-live-p window)
                   (not (flash-emacs--buffer-excluded-p (window-buffer window))))
          (setq conflicting-labels 
                (append conflicting-labels 
                        (flash-emacs--find-conflicting-labels search-pattern 
                                                              (mapcar #'char-to-string
                                                                      (string-to-list labels))
                                                              window)))))
      ;; Remove conflicting labels and their case variants
      (let ((label-chars (string-to-list labels)))
        (mapconcat #'char-to-string
                   (cl-remove-if (lambda (label-char)
                                   (let ((label-str (char-to-string label-char)))
                                     (or (cl-find label-str conflicting-labels :test #'string=)
                                         ;; Also remove the opposite case
                                         (cl-find (if (= label-char (upcase label-char))
                                                      (downcase label-str)
                                                    (upcase label-str))
                                                  conflicting-labels :test #'string=))))
                                 label-chars)
                   "")))))

(defun flash-emacs--distance-from-cursor (match current-point)
  "Calculate distance of MATCH from cursor at CURRENT-POINT."
  (let ((match-pos (plist-get match :pos)))
    (abs (- match-pos current-point))))

(defun flash-emacs--sort-matches (matches current-point current-window)
  "Sort MATCHES by window priority (current window first) then by distance from CURRENT-POINT.
When same buffer is shown in multiple windows, strongly prioritize current window."
  (sort matches
        (lambda (a b)
          (let ((a-window (plist-get a :window))
                (b-window (plist-get b :window))
                (a-buffer (plist-get a :buffer))
                (b-buffer (plist-get b :buffer))
                (current-buffer (window-buffer current-window))
                (a-distance (flash-emacs--distance-from-cursor a current-point))
                (b-distance (flash-emacs--distance-from-cursor b current-point)))
            ;; First priority: current window
            (cond
             ;; Both in current window - sort by distance
             ((and (eq a-window current-window) (eq b-window current-window))
              (< a-distance b-distance))
             ;; A in current window, B not - A wins
             ((eq a-window current-window)
              t)
             ;; B in current window, A not - B wins
             ((eq b-window current-window)
              nil)
             ;; Special case: both matches are in current buffer but different windows
             ;; Prioritize the one that would keep us in current window
             ((and (eq a-buffer current-buffer) (eq b-buffer current-buffer)
                   (not (eq a-window current-window)) (not (eq b-window current-window)))
              ;; Both are in current buffer but different windows - sort by distance
              (< a-distance b-distance))
             ;; A is in current buffer, B is not - A wins
             ((eq a-buffer current-buffer)
              t)
             ;; B is in current buffer, A is not - B wins
             ((eq b-buffer current-buffer)
              nil)
             ;; Neither in current window or buffer - sort by distance
             (t
              (< a-distance b-distance)))))))

(defun flash-emacs--assign-labels (matches labels current-point pattern windows current-window)
  "Assign single-character labels to MATCHES using a two-pass approach for stability.
First pass tries to reuse existing labels, second pass assigns new labels.
Prioritizes matches in CURRENT-WINDOW."
  ;; Initialize label position tracking
  (unless flash-emacs--label-positions
    (setq flash-emacs--label-positions (make-hash-table :test 'equal)))
  
  ;; Reset if pattern changed significantly
  (flash-emacs--reset-label-positions pattern)
  
  (let* ((filtered-labels (flash-emacs--filter-labels-for-pattern labels pattern windows))
         (sorted-matches (flash-emacs--sort-matches matches current-point current-window))
         (available-labels (mapcar #'char-to-string (string-to-list filtered-labels)))
         (labeled-matches '())
         (used-labels '()))
    
    ;; First pass: Try to reuse existing labels for positions that still exist
    (dolist (match sorted-matches)
      (let* ((pos-key (flash-emacs--make-position-key match))
             (existing-label (gethash pos-key flash-emacs--label-positions)))
        (when (and existing-label 
                   (member existing-label available-labels)
                   (not (member existing-label used-labels))
                   (flash-emacs--can-reuse-label-p existing-label))
          (plist-put match :label existing-label)
          (push existing-label used-labels)
          (setq available-labels (delete existing-label available-labels))
          (push match labeled-matches))))
    
    ;; Second pass: Assign new labels to unassigned matches
    (dolist (match sorted-matches)
      (when (and (not (plist-get match :label))  ; Not labeled in first pass
                 available-labels)                ; Labels still available
        (let ((new-label (car available-labels)))
          (plist-put match :label new-label)
          (push new-label used-labels)
          (setq available-labels (cdr available-labels))
          ;; Remember this assignment for future iterations  
          (when (flash-emacs--can-reuse-label-p new-label)
            (puthash (flash-emacs--make-position-key match) new-label flash-emacs--label-positions))
          (push match labeled-matches))))
    
    (nreverse labeled-matches)))

(defun flash-emacs--make-position-key (match)
  "Create a unique position key for MATCH to track label reuse.
The key includes buffer and position information."
  (let ((buffer (plist-get match :buffer))
        (pos (plist-get match :pos)))
    (format "%s:%d" 
            (buffer-name buffer)
            pos)))

(defun flash-emacs--reset-label-positions (pattern)
  "Reset label position tracking if pattern changed significantly.
This maintains stability when refining searches but resets on new searches."
  (when (or (not flash-emacs--current-pattern)
            (not (string-prefix-p flash-emacs--current-pattern pattern))
            (< (length pattern) (length (or flash-emacs--current-pattern ""))))
    (setq flash-emacs--label-positions (make-hash-table :test 'equal)))
  (setq flash-emacs--current-pattern pattern))

(defun flash-emacs--can-reuse-label-p (label)
  "Return non-nil if LABEL can be reused based on configuration."
  (cond
   ((eq flash-emacs-label-reuse 'none) nil)
   ((eq flash-emacs-label-reuse 'all) t)
   ((eq flash-emacs-label-reuse 'lowercase)
    (string= label (downcase label)))
   (t nil)))

;;; Visual feedback

(defun flash-emacs--create-label-overlay (match)
  "Create an overlay for the label of MATCH."
  (let* ((pos (plist-get match :pos))
         (buffer (plist-get match :buffer))
         (label (plist-get match :label)))
    (when label
      (with-current-buffer buffer
        (let ((overlay (make-overlay pos (1+ pos))))
          (overlay-put overlay 'display 
                       (propertize label 'face 'flash-emacs-label))
          (overlay-put overlay 'flash-emacs 'label)
          overlay)))))

(defun flash-emacs--create-match-overlay (match)
  "Create an overlay for highlighting MATCH."
  (let* ((pos (plist-get match :pos))
         (end-pos (plist-get match :end-pos))
         (buffer (plist-get match :buffer)))
    (with-current-buffer buffer
      (let ((overlay (make-overlay pos end-pos)))
        (overlay-put overlay 'face 'flash-emacs-match)
        (overlay-put overlay 'flash-emacs 'match)
        overlay))))

(defun flash-emacs--show-overlays (all-matches labeled-matches)
  "Display overlays for ALL-MATCHES (background) and LABELED-MATCHES (labels)."
  (flash-emacs--clear-overlays)
  ;; Show background highlighting for ALL matches
  (dolist (match all-matches)
    (when-let* ((match-overlay (flash-emacs--create-match-overlay match)))
      (push match-overlay flash-emacs--overlays)))
  ;; Show labels only for labeled matches
  (dolist (match labeled-matches)
    (when-let* ((label-overlay (flash-emacs--create-label-overlay match)))
      (push label-overlay flash-emacs--overlays))))

(defun flash-emacs--clear-overlays ()
  "Remove all flash overlays."
  (dolist (overlay flash-emacs--overlays)
    (delete-overlay overlay))
  (setq flash-emacs--overlays nil))

;;; Input handling

(defun flash-emacs--find-match-by-label (label matches)
  "Find the match with the given LABEL in MATCHES."
  (cl-find-if (lambda (match)
                (string= (plist-get match :label) label))
              matches))

(defvar flash-emacs-jump-hook nil
  "Hook run after a successful flash jump.
Each hook function is called with the selected match.")

(defun flash-emacs--jump-to-match (match)
  "Jump to the position of MATCH.
Prioritizes staying in current window if the target buffer is already displayed there."
  (let ((target-window (plist-get match :window))
        (pos (plist-get match :pos)))

    (push-mark)
    (select-window target-window)
    (goto-char pos)

    ;; Run hooks after jump
    (run-hook-with-args 'flash-emacs-jump-hook match)))

;;; Main function

;;;###autoload
(defun flash-emacs-jump ()
  "Start flash jump mode - simple version."
  (interactive)
  ;; Reset label positions for new session
  (setq flash-emacs--label-positions (make-hash-table :test 'equal)
        flash-emacs--current-pattern nil)
  
  ;; Suppress all messages during flash operation to prevent "mark set" and other unwanted messages
  (let ((original-message (symbol-function 'message)))
    (cl-letf (((symbol-function 'message) 
               (lambda (format-string &rest args)
                 ;; Only allow specific flash-related messages
                 (when (and format-string 
                            (string-match-p "\\(Flash\\|cancelled\\)" format-string))
                   (apply original-message format-string args)))))
      
      (let ((pattern "")
            (matches '())
            (labeled-matches '()))
        
        (unwind-protect
            (catch 'flash-exit
              (while t
                ;; Get input
                (let* ((prompt (if (> (length pattern) 0)
                                   (concat "Flash:" pattern)
                                 "Flash:"))
                       (char (read-char-exclusive prompt)))
                  
                  (cond
                   ;; ESC or C-g - exit
                   ((or (= char 27) (= char 7))
                    (funcall original-message "Flash cancelled")
                    (throw 'flash-exit nil))
                   
                   ;; Enter - jump to first match
                   ((= char 13)
                    (when (car labeled-matches)
                      (flash-emacs--jump-to-match (car labeled-matches)))
                    (throw 'flash-exit nil))
                   
                   ;; Backspace - remove last character
                   ((or (= char 127) (= char 8))
                    (if (> (length pattern) 0)
                        (setq pattern (substring pattern 0 -1))
                      (throw 'flash-exit nil)))
                   
                   ;; Check if it's a jump label
                   ((and (>= char 32) (<= char 126))  ; Printable ASCII
                    (let* ((new-char (char-to-string char))
                           (target-match (flash-emacs--find-match-by-label new-char labeled-matches)))
                      (if target-match
                          ;; It's a jump label - perform the jump
                          (progn
                            (flash-emacs--jump-to-match target-match)
                            (throw 'flash-exit nil))
                        ;; It's a regular character - add to search pattern
                        (setq pattern (concat pattern new-char)))))
                   
                   ;; Unknown character - ignore
                   (t nil))
                  
                  ;; Update search results
                  (setq matches (flash-emacs--search-pattern pattern))
                  
                  ;; Check for zero matches and exit automatically
                  (when (and (> (length pattern) 0) (= (length matches) 0))
                    ;; Don't show "no matches" message during interactive search
                    (throw 'flash-exit nil))

                  ;; Auto-jump if exactly one match and option is enabled
                  (when (and flash-emacs-auto-jump-single
                             (= (length matches) 1))
                    (flash-emacs--jump-to-match (car matches))
                    (throw 'flash-exit nil))

                  (let ((windows (if flash-emacs-multi-window
                                     (window-list)
                                   (list (selected-window))))
                        (current-window (selected-window)))
                    (setq labeled-matches (flash-emacs--assign-labels matches flash-emacs-labels (point) pattern windows current-window)))
                  (flash-emacs--show-overlays matches labeled-matches))))
          
          ;; Cleanup
          (flash-emacs--clear-overlays))))))

(provide 'flash-emacs)

;;; flash-emacs.el ends here
