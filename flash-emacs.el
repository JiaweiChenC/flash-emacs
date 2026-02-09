;;; flash-emacs.el --- Simple flash.nvim-like jump navigation for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Jiawei Chen
;; Maintainer: Jiawei Chen
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

(defcustom flash-emacs-labels "asdghklqwertyuiopzxcvbnmfjASDGHKLQWERTYUIOPZXCVBNMFJ"
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
- \\='smart: case-insensitive if pattern is all lowercase, case-sensitive if it contains uppercase"
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

(defcustom flash-emacs-dim-background nil
  "Whether to dim the background during jump navigation."
  :type 'boolean
  :group 'flash-emacs)

(defcustom flash-emacs-label-position 'after
  "Where to place jump labels relative to the match."
  :type '(choice (const :tag "After the match" after)
          (const :tag "Before the match" before))
  :group 'flash-emacs)

(defcustom flash-emacs-label-style 'replace
  "How to display jump labels.
- \\='inline: Label appears as virtual text without replacing any characters (flash.nvim style)
- \\='replace: Label replaces the character at the label position"
  :type '(choice (const :tag "Inline virtual text (flash.nvim style)" inline)
          (const :tag "Replace character at position" replace))
  :group 'flash-emacs)

(defcustom flash-emacs-jump-position 'start
  "Where to place the cursor after jumping to a match.
- \\='label: Jump to where the label is displayed (follows `flash-emacs-label-position')
- \\='start: Always jump to the start of the match
- \\='end: Always jump to the end of the match"
  :type '(choice (const :tag "Where label is displayed" label)
          (const :tag "Start of match" start)
          (const :tag "End of match" end))
  :group 'flash-emacs)

(defcustom flash-emacs-exclude-modes
  '(image-mode pdf-view-mode doc-view-mode archive-mode tar-mode hexl-mode binary-mode)
  "List of major modes to exclude from search.
These modes typically contain binary data or non-textual content."
  :type '(repeat symbol)
  :group 'flash-emacs)

(defcustom flash-emacs-exclude-functions
  '(flash-emacs--buffer-binary-p flash-emacs--buffer-too-large-p)
  "List of functions to determine if a buffer should be excluded.
Each function should take a buffer as argument and return non-nil to exclude it."
  :type '(repeat function)
  :group 'flash-emacs)

(defcustom flash-emacs-max-buffer-size 1048576
  "Maximum buffer size to search in bytes.
Buffers larger than this are excluded to avoid performance issues."
  :type 'integer
  :group 'flash-emacs)

(defcustom flash-emacs-label-reuse 'lowercase
  "How to reuse labels for positions during search refinement.
- \\='none: never reuse labels, assign new ones each time
- \\='lowercase: reuse only lowercase labels (default)
- \\='all: reuse both lowercase and uppercase labels
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

(defface flash-emacs-dim-face
  '((t (:inherit (shadow font-lock-comment-face)
        :weight unspecified
        :slant unspecified
        :underline nil
        :strike-through nil
        :background unspecified)))
  "Face used to dim the background text during flash-emacs-jump."
  :group 'flash-emacs)

;;; Internal variables

(defvar flash-emacs--overlays nil
  "List of active overlays.")

(defvar flash-emacs--dim-overlays nil
  "List of background dimming overlays.")

(defvar flash-emacs--label-positions nil
  "Hash table mapping position keys to previously assigned labels.")

(defvar flash-emacs--current-pattern nil
  "Current search pattern for tracking pattern changes.")

;;; Utility functions

(defun flash-emacs--get-windows ()
  "Get list of windows to search in based on configuration."
  (if flash-emacs-multi-window (window-list) (list (selected-window))))

(defun flash-emacs--should-ignore-case (pattern)
  "Determine if search should ignore case based on PATTERN and settings."
  (pcase flash-emacs-case-sensitive
    ('nil t)
    ('t nil)
    ('smart (string= pattern (downcase pattern)))
    (_ t)))

;;; Buffer exclusion functions

(defun flash-emacs--buffer-binary-p (buffer)
  "Return non-nil if BUFFER contains binary data."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((sample-size (min 8192 (buffer-size))))
        (when (> sample-size 0)
          (or (search-forward "\0" (+ (point-min) sample-size) t)
              (let ((non-printable 0)
                    (total 0))
                (while (and (< (point) (+ (point-min) sample-size)) (not (eobp)))
                  (when-let* ((char (char-after)))
                    (cl-incf total)
                    (when (and (< char 32) (not (memq char '(?\t ?\n ?\r))))
                      (cl-incf non-printable)))
                  (forward-char 1))
                (and (> total 0) (> (/ (* non-printable 100) total) 30)))))))))

(defun flash-emacs--buffer-too-large-p (buffer)
  "Return non-nil if BUFFER is too large for efficient searching."
  (> (buffer-size buffer) flash-emacs-max-buffer-size))

(defun flash-emacs--buffer-excluded-p (buffer)
  "Return non-nil if BUFFER should be excluded from search."
  (with-current-buffer buffer
    (let ((name (buffer-name)))
      (or (memq major-mode flash-emacs-exclude-modes)
          (cl-some (lambda (fn) (and (functionp fn) (funcall fn buffer)))
                   flash-emacs-exclude-functions)
          (and (string-prefix-p " " name) (< (buffer-size) 10))
          (and (string-prefix-p "*" name)
               (not (string= "*scratch*" name))
               (not (string-match "\\*.*\\*<[0-9]+>$" name))
               (not (buffer-file-name))
               (string-match "^\\*\\(Messages\\|Completions\\|Help\\|Warnings\\|Backtrace\\|Compile-Log\\)" name))))))

;;; Search functions

(defun flash-emacs--get-window-bounds (window)
  "Get visible (start-pos . end-pos) bounds for WINDOW."
  (cons (window-start window) (window-end window)))

(defun flash-emacs--search-in-window (pattern window)
  "Search for PATTERN in WINDOW and return list of matches."
  (when-let* ((buffer (window-buffer window))
              ((not (flash-emacs--buffer-excluded-p buffer))))
    (let ((case-fold-search (flash-emacs--should-ignore-case pattern))
          (bounds (flash-emacs--get-window-bounds window))
          (matches '()))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (car bounds))
          (while (search-forward pattern (cdr bounds) t)
            (push (list :pos (match-beginning 0)
                        :end-pos (match-end 0)
                        :window window
                        :buffer buffer
                        :text (match-string-no-properties 0))
                  matches))))
      (nreverse matches))))

(defun flash-emacs--search-pattern (pattern)
  "Find all matches for PATTERN in visible windows."
  (when (>= (length pattern) flash-emacs-min-pattern-length)
    (cl-loop for window in (flash-emacs--get-windows)
             when (window-live-p window)
             append (flash-emacs--search-in-window pattern window))))

;;; Label assignment

(defun flash-emacs--create-skip-pattern (search-pattern)
  "Create a skip pattern for SEARCH-PATTERN to avoid label conflicts."
  (when (and search-pattern (> (length search-pattern) 0))
    (concat (regexp-quote search-pattern) ".")))

(defun flash-emacs--find-conflicting-labels (search-pattern labels window)
  "Find labels that would conflict with continuing SEARCH-PATTERN in WINDOW."
  (when-let* ((buffer (window-buffer window))
              ((not (flash-emacs--buffer-excluded-p buffer)))
              (skip-pattern (flash-emacs--create-skip-pattern search-pattern)))
    (let ((case-fold-search (flash-emacs--should-ignore-case search-pattern))
          (bounds (flash-emacs--get-window-bounds window))
          (conflicts '()))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (car bounds))
          (while (re-search-forward skip-pattern (cdr bounds) t)
            (let ((following-char (buffer-substring-no-properties (1- (match-end 0)) (match-end 0))))
              (when-let* ((matched (cl-find-if
                                    (lambda (label)
                                      (if case-fold-search
                                          (string= (downcase following-char) (downcase label))
                                        (string= following-char label)))
                                    labels)))
                (push matched conflicts))))))
      (delete-dups conflicts))))

(defun flash-emacs--filter-labels-for-pattern (labels search-pattern windows)
  "Filter out labels that would conflict with SEARCH-PATTERN continuation."
  (if (or (not search-pattern) (zerop (length search-pattern)))
      labels
    (let* ((label-strings (mapcar #'char-to-string (string-to-list labels)))
           (conflicts (cl-loop for window in windows
                               when (window-live-p window)
                               append (flash-emacs--find-conflicting-labels
                                       search-pattern label-strings window))))
      (mapconcat #'char-to-string
                 (cl-remove-if
                  (lambda (char)
                    (let ((str (char-to-string char)))
                      (or (member str conflicts)
                          (member (if (= char (upcase char))
                                      (downcase str)
                                    (upcase str))
                                  conflicts))))
                  (string-to-list labels))
                 ""))))

(defun flash-emacs--distance-from-cursor (match current-point)
  "Calculate distance of MATCH from CURRENT-POINT."
  (abs (- (plist-get match :pos) current-point)))

(defun flash-emacs--sort-matches (matches current-point current-window)
  "Sort MATCHES by window priority then distance from CURRENT-POINT."
  (let ((current-buffer (window-buffer current-window)))
    (sort matches
          (lambda (a b)
            (let ((a-win (plist-get a :window))
                  (b-win (plist-get b :window))
                  (a-buf (plist-get a :buffer))
                  (b-buf (plist-get b :buffer))
                  (a-dist (flash-emacs--distance-from-cursor a current-point))
                  (b-dist (flash-emacs--distance-from-cursor b current-point)))
              (cond
               ((and (eq a-win current-window) (eq b-win current-window))
                (< a-dist b-dist))
               ((eq a-win current-window) t)
               ((eq b-win current-window) nil)
               ((and (eq a-buf current-buffer) (eq b-buf current-buffer))
                (< a-dist b-dist))
               ((eq a-buf current-buffer) t)
               ((eq b-buf current-buffer) nil)
               (t (< a-dist b-dist))))))))

(defun flash-emacs--make-position-key (match)
  "Create a unique position key for MATCH."
  (cons (plist-get match :window) (plist-get match :pos)))

(defun flash-emacs--can-reuse-label-p (label)
  "Return non-nil if LABEL can be reused based on configuration."
  (pcase flash-emacs-label-reuse
    ('none nil)
    ('all t)
    ('lowercase (string= label (downcase label)))
    (_ nil)))

(defun flash-emacs--reset-label-positions (pattern)
  "Reset label position tracking if PATTERN changed significantly."
  (when (or (not flash-emacs--current-pattern)
            (not (string-prefix-p flash-emacs--current-pattern pattern))
            (< (length pattern) (length (or flash-emacs--current-pattern ""))))
    (setq flash-emacs--label-positions (make-hash-table :test 'equal)))
  (setq flash-emacs--current-pattern pattern))

(defun flash-emacs--assign-labels (matches labels current-point pattern windows current-window)
  "Assign labels to MATCHES using a two-pass approach for stability."
  (unless flash-emacs--label-positions
    (setq flash-emacs--label-positions (make-hash-table :test 'equal)))
  (flash-emacs--reset-label-positions pattern)
  (let* ((filtered-labels (flash-emacs--filter-labels-for-pattern labels pattern windows))
         (sorted-matches (flash-emacs--sort-matches matches current-point current-window))
         (available (mapcar #'char-to-string (string-to-list filtered-labels)))
         (labeled '()))
    ;; First pass: reuse existing labels
    (dolist (match sorted-matches)
      (when-let* ((pos-key (flash-emacs--make-position-key match))
                  (existing (gethash pos-key flash-emacs--label-positions))
                  ((member existing available))
                  ((flash-emacs--can-reuse-label-p existing)))
        (plist-put match :label existing)
        (setq available (delete existing available))
        (push match labeled)))
    ;; Second pass: assign new labels
    (dolist (match sorted-matches)
      (when (and (not (plist-get match :label)) available)
        (let ((new-label (pop available)))
          (plist-put match :label new-label)
          (when (flash-emacs--can-reuse-label-p new-label)
            (puthash (flash-emacs--make-position-key match) new-label flash-emacs--label-positions))
          (push match labeled))))
    (nreverse labeled)))

;;; Visual feedback

(defun flash-emacs--make-overlay (start end &rest properties)
  "Create overlay from START to END with PROPERTIES."
  (let ((ov (make-overlay start end)))
    (cl-loop for (prop val) on properties by #'cddr
             do (overlay-put ov prop val))
    ov))

(defun flash-emacs--create-label-overlay (match)
  "Create an overlay for the label of MATCH."
  (when-let* ((label (plist-get match :label))
              (buffer (plist-get match :buffer))
              (window (plist-get match :window))
              (match-start (plist-get match :pos))
              (match-end (plist-get match :end-pos))
              (styled-label (propertize label 'face 'flash-emacs-label)))
    (with-current-buffer buffer
      (pcase flash-emacs-label-style
        ('inline
          (let ((ov (make-overlay (if (eq flash-emacs-label-position 'after) match-end match-start)
                                  (if (eq flash-emacs-label-position 'after) match-end match-start))))
            (overlay-put ov (if (eq flash-emacs-label-position 'after) 'after-string 'before-string)
                         styled-label)
            (overlay-put ov 'flash-emacs 'label)
            (overlay-put ov 'priority 200)
            (overlay-put ov 'window window)
            ov))
        (_  ; replace style
         (let* ((target-pos (if (eq flash-emacs-label-position 'after) match-end match-start))
                (char-at-target (char-after target-pos))
                (at-newline-or-eob (or (null char-at-target) (= char-at-target ?\n))))
           (if at-newline-or-eob
               (flash-emacs--make-overlay target-pos target-pos
                                          'before-string styled-label
                                          'flash-emacs 'label
                                          'priority 200
                                          'window window)
             (flash-emacs--make-overlay target-pos (1+ target-pos)
                                        'display styled-label
                                        'flash-emacs 'label
                                        'priority 200
                                        'window window))))))))

(defun flash-emacs--create-match-overlay (match)
  "Create an overlay for highlighting MATCH."
  (with-current-buffer (plist-get match :buffer)
    (flash-emacs--make-overlay (plist-get match :pos) (plist-get match :end-pos)
                               'face 'flash-emacs-match
                               'flash-emacs 'match
                               'priority 150)))

(defun flash-emacs--dim-windows ()
  "Apply dimming overlays to visible areas of searched windows."
  (when flash-emacs-dim-background
    (dolist (wnd (flash-emacs--get-windows))
      (with-selected-window wnd
        (push (flash-emacs--make-overlay (window-start) (window-end)
                                         'window wnd
                                         'face 'flash-emacs-dim-face
                                         'priority 10)
              flash-emacs--dim-overlays)))))

(defun flash-emacs--show-overlays (all-matches labeled-matches)
  "Display overlays for ALL-MATCHES and LABELED-MATCHES."
  (flash-emacs--clear-overlays)
  (dolist (match all-matches)
    (push (flash-emacs--create-match-overlay match) flash-emacs--overlays))
  (dolist (match labeled-matches)
    (when-let* ((ov (flash-emacs--create-label-overlay match)))
      (push ov flash-emacs--overlays))))

(defun flash-emacs--clear-overlays ()
  "Remove all flash overlays."
  (mapc #'delete-overlay flash-emacs--overlays)
  (setq flash-emacs--overlays nil))

(defun flash-emacs--clear-dim-overlays ()
  "Remove all background dimming overlays."
  (mapc #'delete-overlay flash-emacs--dim-overlays)
  (setq flash-emacs--dim-overlays nil))

;;; Input handling

(defun flash-emacs--find-match-by-label (label matches)
  "Find the match with LABEL in MATCHES."
  (cl-find label matches :key (lambda (m) (plist-get m :label)) :test #'string=))

(defvar flash-emacs-jump-hook nil
  "Hook run after a successful flash jump.")

(defun flash-emacs--calculate-jump-position (match)
  "Calculate the jump position for MATCH."
  (pcase flash-emacs-jump-position
    ('start (plist-get match :pos))
    ('end (plist-get match :end-pos))
    (_ (if (eq flash-emacs-label-position 'after)
           (plist-get match :end-pos)
         (plist-get match :pos)))))

(defvar evil-state)
(defvar evil-visual-selection)

(defun flash-emacs--evil-visual-mode-p ()
  "Return non-nil if in Evil visual mode."
  (and (boundp 'evil-state) (eq evil-state 'visual)))

(defun flash-emacs--evil-visual-selection-type ()
  "Return the Evil visual selection type (char, line, block) or nil."
  (when (flash-emacs--evil-visual-mode-p)
    (and (boundp 'evil-visual-selection)
         (or evil-visual-selection 'char))))

(defun flash-emacs--jump-to-match (match)
  "Jump to the position of MATCH."
  (let* ((target-window (plist-get match :window))
         (pos (flash-emacs--calculate-jump-position match))
         (visual-mode (flash-emacs--evil-visual-mode-p))
         (visual-type (flash-emacs--evil-visual-selection-type))
         (original-mark (when visual-mode (mark t))))
    (unless visual-mode (push-mark))
    (select-window target-window)
    ;; Adjust for Evil visual char/block mode extending forward
    (when (and (memq visual-type '(char block nil))
               original-mark
               (> pos original-mark)
               (< pos (point-max)))
      (cl-incf pos))
    (goto-char pos)
    ;; Recreate visual line selection if needed
    (when (and (eq visual-type 'line)
               original-mark
               (fboundp 'evil-visual-make-selection))
      (evil-visual-make-selection original-mark (point) 'line))
    (run-hook-with-args 'flash-emacs-jump-hook match)))

;;; Input handling for main loop

(defun flash-emacs--handle-input (char pattern labeled-matches original-message)
  "Handle input CHAR with current PATTERN and LABELED-MATCHES.
ORIGINAL-MESSAGE is the original message function.
Returns (action . value) where action is exit, backspace, add-char, or nil."
  (cond
   ;; ESC or C-g
   ((memq char '(27 7))
    (funcall original-message "Flash cancelled")
    (cons 'exit nil))
   ;; Enter - jump to first match
   ((= char 13)
    (when-let* ((first-match (car labeled-matches)))
      (flash-emacs--jump-to-match first-match))
    (cons 'exit nil))
   ;; Backspace
   ((memq char '(127 8))
    (if (> (length pattern) 0)
        (cons 'backspace (substring pattern 0 -1))
      (cons 'exit nil)))
   ;; Printable ASCII
   ((and (>= char 32) (<= char 126))
    (let ((char-str (char-to-string char)))
      (if-let* ((target (flash-emacs--find-match-by-label char-str labeled-matches)))
          (progn (flash-emacs--jump-to-match target)
                 (cons 'exit nil))
        (cons 'add-char (concat pattern char-str)))))
   (t nil)))

;;; Main function

;;;###autoload
(defun flash-emacs-jump ()
  "Start flash jump mode."
  (interactive)
  (setq flash-emacs--label-positions (make-hash-table :test 'equal)
        flash-emacs--current-pattern nil)
  (let ((original-message (symbol-function 'message)))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when (and fmt (string-match-p "\\(Flash\\|cancelled\\)" fmt))
                   (apply original-message fmt args)))))
      (let ((pattern "")
            (matches '())
            (labeled-matches '()))
        (unwind-protect
            (catch 'flash-exit
              (flash-emacs--dim-windows)
              (while t
                (let* ((prompt (concat "Flash:" (if (> (length pattern) 0) pattern "")))
                       (result (flash-emacs--handle-input
                                (read-char-exclusive prompt)
                                pattern labeled-matches original-message)))
                  (pcase result
                    (`(exit . ,_) (throw 'flash-exit nil))
                    (`(backspace . ,new-pattern) (setq pattern new-pattern))
                    (`(add-char . ,new-pattern) (setq pattern new-pattern)))
                  ;; Update search
                  (setq matches (flash-emacs--search-pattern pattern))
                  ;; Exit if no matches
                  (when (and (> (length pattern) 0) (null matches))
                    (throw 'flash-exit nil))
                  ;; Auto-jump if single match
                  (when (and flash-emacs-auto-jump-single (= (length matches) 1))
                    (flash-emacs--jump-to-match (car matches))
                    (throw 'flash-exit nil))
                  ;; Update labels and display
                  (setq labeled-matches
                        (flash-emacs--assign-labels matches flash-emacs-labels (point)
                                                    pattern (flash-emacs--get-windows)
                                                    (selected-window)))
                  (flash-emacs--show-overlays matches labeled-matches))))
          (flash-emacs--clear-overlays)
          (flash-emacs--clear-dim-overlays))))))

(provide 'flash-emacs)

;;; flash-emacs.el ends here
