;;; flash-emacs-search.el --- Search integration for flash-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Jiawei Chen
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (flash-emacs "1.0.0"))
;; Keywords: navigation, jump, search, convenience
;; URL: https://github.com/flash-emacs/flash-emacs

;;; Commentary:

;; Search integration for flash-emacs, inspired by flash.nvim.
;;
;; This integrates flash-emacs labels with Evil's / and ? search.
;; When enabled, labels appear next to search matches as you type,
;; allowing you to jump directly to any match by pressing its label.
;;
;; Usage:
;;   (require 'flash-emacs-search)
;;
;; Search integration is enabled automatically on load.
;; Use Evil's / or ? to search. Labels will appear automatically.
;; Press a label key to jump to that match and exit search.
;; Toggle with C-s during search to enable/disable labels.
;;
;; To disable: (flash-emacs-search-disable)
;; To re-enable: (flash-emacs-search-enable)
;; To control whether labels show by default: (setq flash-emacs-search-enabled nil)

;;; Code:

(require 'flash-emacs)

(eval-when-compile
  (require 'evil nil t))

;;; Customization

(defgroup flash-emacs-search nil
  "Search integration for flash-emacs."
  :group 'flash-emacs
  :prefix "flash-emacs-search-")

(defcustom flash-emacs-search-enabled t
  "Whether flash labels are shown during search by default."
  :type 'boolean
  :group 'flash-emacs-search)

(defcustom flash-emacs-search-toggle-key "C-s"
  "Key to toggle flash labels during search."
  :type 'string
  :group 'flash-emacs-search)

(defcustom flash-emacs-search-min-length 1
  "Minimum search pattern length before showing labels."
  :type 'integer
  :group 'flash-emacs-search)

;;; Evil variables

(defvar evil-ex-search-pattern)
(defvar evil-ex-search-direction)
(defvar evil-ex-search-start-point)
(defvar evil-ex-original-buffer)
(defvar evil-ex-search-keymap)
(declare-function evil-ex-pattern-regex "evil-search")
(declare-function evil-ex-delete-hl "evil-search")

;;; Internal variables

(defvar flash-emacs-search--active nil
  "Whether flash labels are currently active.")

(defvar flash-emacs-search--overlays nil
  "List of flash search overlays.")

(defvar flash-emacs-search--all-matches nil
  "All labeled matches for visible area.")

(defvar flash-emacs-search--current-pattern nil
  "Current search pattern (to detect changes).")

(defvar flash-emacs-search--original-buffer nil
  "Buffer where search started.")

(defvar flash-emacs-search--used-labels nil
  "Hash table mapping positions to assigned labels (persists during search session).")

(defvar flash-emacs-search--updating nil
  "Non-nil when labels are being updated. Prevents re-entrant refresh.")

(defvar flash-emacs-search--last-update-time nil
  "Time of last label update. Used to suppress scroll-triggered refreshes.")

(defvar flash-emacs-search--available-labels nil
  "List of labels still available to assign.")

;;; Overlay management

(defun flash-emacs-search--clear-overlays ()
  "Clear all flash search overlays."
  (mapc #'delete-overlay flash-emacs-search--overlays)
  (setq flash-emacs-search--overlays nil))

(defun flash-emacs-search--clear-all ()
  "Clear overlays and all match data."
  (flash-emacs-search--clear-overlays)
  (setq flash-emacs-search--all-matches nil
        flash-emacs-search--current-pattern nil
        flash-emacs-search--used-labels nil
        flash-emacs-search--available-labels nil
        flash-emacs-search--conflict-cache nil
        flash-emacs-search--updating nil
        flash-emacs-search--last-update-time nil))

(defun flash-emacs-search--in-image-overlay-p (pos)
  "Check if POS is inside an org image overlay (including sliced images).
This prevents flash labels from disrupting image display."
  (cl-some (lambda (ov)
             (overlay-get ov 'org-image-overlay))
           (overlays-at pos)))

(defun flash-emacs-search--create-label-overlay (end-pos label win)
  "Create a label overlay at END-POS (after match) with LABEL in window WIN.
Uses replace style - the label replaces the character after the match.
Returns nil if position is inside an org image overlay."
  (with-selected-window win
    ;; Skip if inside an org image overlay (e.g., org-sliced-images)
    (unless (flash-emacs-search--in-image-overlay-p end-pos)
      (let* ((label-str (propertize label 'face 'flash-emacs-label))
             (char-at-pos (char-after end-pos))
             (at-newline-or-eob (or (null char-at-pos) (= char-at-pos ?\n)))
             (ov (if at-newline-or-eob
                     ;; At newline or end of buffer, use after-string with high priority
                     ;; This ensures flash labels appear BEFORE other after-string overlays
                     ;; (like search count overlays) due to higher priority
                     (let ((o (make-overlay end-pos end-pos)))
                       (overlay-put o 'after-string label-str)
                       o)
                   ;; Replace the character after the match
                   (let ((o (make-overlay end-pos (1+ end-pos))))
                     (overlay-put o 'display label-str)
                     o))))
        (overlay-put ov 'flash-emacs-search t)
        ;; Very high priority so flash labels appear before other overlays
        ;; at same position (like search count [x/y] overlays)
        (overlay-put ov 'priority 20000)
        (overlay-put ov 'window win)
        ov))))

;;; Window helpers

(defun flash-emacs-search--get-windows ()
  "Get windows for search, excluding minibuffer.
Always includes the main buffer window, regardless of `flash-emacs-multi-window'."
  (let ((wins (cl-remove-if (lambda (win)
                              (or (minibufferp (window-buffer win))
                                  (not (window-live-p win))))
                            (if flash-emacs-multi-window
                                (window-list)
                              ;; When multi-window is nil, selected-window may be
                              ;; the minibuffer during search. Use minibuffer-selected-window
                              ;; as fallback.
                              (let ((main-win (or (minibuffer-selected-window)
                                                  (selected-window))))
                                (list main-win))))))
    wins))

;;; Match finding

(defun flash-emacs-search--find-visible-matches (pattern)
  "Find matches for PATTERN in the VISIBLE area only (like flash.nvim).
PATTERN is treated as a literal string for searching.
Skips matches inside org image overlays (e.g., org-sliced-images)."
  (when (and pattern (>= (length pattern) flash-emacs-search-min-length))
    (let ((matches '())
          (case-fold-search (flash-emacs--should-ignore-case pattern))
          (search-regexp (regexp-quote pattern))
          (win (or (minibuffer-selected-window) (selected-window))))
      (with-selected-window win
        (let ((win-start (window-start win))
              (win-end (window-end win t)))
          (save-excursion
            (goto-char win-start)
            (condition-case nil
                (while (and (re-search-forward search-regexp nil t)
                            (<= (match-beginning 0) win-end))
                  (let ((start (match-beginning 0))
                        (end (match-end 0)))
                    ;; Skip matches inside org image overlays
                    (unless (or (= start end)
                                (flash-emacs-search--in-image-overlay-p start)
                                (flash-emacs-search--in-image-overlay-p end))
                      (push (list :pos start
                                  :end-pos end
                                  :window win
                                  :buffer (current-buffer))
                            matches))))
              (invalid-regexp nil)))))
      (nreverse matches))))

;;; Label assignment

(defvar flash-emacs-search--conflict-cache nil
  "Cache for conflict detection: (pattern . conflicts).")

(defun flash-emacs-search--find-buffer-conflicts (pattern labels)
  "Find labels that conflict with PATTERN continuation.
Searches the entire buffer for pattern+label sequences in a single pass.
Results are cached per pattern."
  (when (and pattern (> (length pattern) 0) labels)
    ;; Check cache first
    (if (and flash-emacs-search--conflict-cache
             (equal (car flash-emacs-search--conflict-cache) pattern))
        (cdr flash-emacs-search--conflict-cache)
      ;; Single-pass approach: search for pattern + [all-labels] once
      (let* ((case-fold-search (flash-emacs--should-ignore-case pattern))
             (label-chars (mapconcat #'identity labels ""))
             (search-regexp (concat (regexp-quote pattern)
                                    "\\(" "[" (regexp-quote label-chars) "]" "\\)"))
             (conflicts '())
             (win (car (flash-emacs-search--get-windows))))
        (when win
          (with-current-buffer (window-buffer win)
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward search-regexp nil t)
                (let ((conflict-char (match-string-no-properties 1)))
                  (unless (member conflict-char conflicts)
                    (push conflict-char conflicts)))))))
        ;; Cache and return
        (setq flash-emacs-search--conflict-cache (cons pattern conflicts))
        conflicts))))

(defun flash-emacs-search--filter-labels (labels pattern)
  "Filter LABELS to remove those conflicting with PATTERN in entire buffer."
  (if (or (not pattern) (zerop (length pattern)))
      labels
    (let* ((label-strings (mapcar #'char-to-string (string-to-list labels)))
           (conflicts (flash-emacs-search--find-buffer-conflicts pattern label-strings)))
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

(defun flash-emacs-search--init-labels (pattern)
  "Initialize available labels for PATTERN (called once per pattern change)."
  (let ((filtered-labels (flash-emacs-search--filter-labels flash-emacs-labels pattern)))
    (setq flash-emacs-search--used-labels (make-hash-table :test 'equal)
          flash-emacs-search--available-labels
          (mapcar #'char-to-string (string-to-list filtered-labels)))))

(defun flash-emacs-search--assign-labels (matches pattern)
  "Assign labels to MATCHES sorted by distance from cursor.

Like flash.nvim: closest matches to cursor get labels first.
The `used-labels` table provides stability when PATTERN changes (typing more
characters), but on cursor movement, labels are redistributed to the closest
matches."
  (let* ((current-window (or (minibuffer-selected-window) (selected-window)))
         (current-point (with-selected-window current-window (point)))
         (sorted-matches (flash-emacs--sort-matches matches current-point current-window)))
    ;; Assign labels to closest matches first
    (dolist (match sorted-matches)
      (when flash-emacs-search--available-labels
        (let* ((pos (plist-get match :pos))
               ;; Try to reuse existing label if available (for pattern stability)
               (existing-label (gethash pos flash-emacs-search--used-labels))
               (label (if (and existing-label
                               (member existing-label flash-emacs-search--available-labels))
                          existing-label
                        (car flash-emacs-search--available-labels))))
          (when label
            (plist-put match :label label)
            (setq flash-emacs-search--available-labels
                  (delete label flash-emacs-search--available-labels))
            ;; Remember for pattern stability
            (puthash pos label flash-emacs-search--used-labels)))))
    ;; Return only matches that got labels
    (cl-remove-if-not (lambda (m) (plist-get m :label)) sorted-matches)))

;;; Display

(defun flash-emacs-search--show-labels (pattern)
  "Find visible matches and assign labels (like flash.nvim).

Key behavior matching flash.nvim:
- Labels are initialized once per pattern (used-labels table persists)
- On each update, available-labels is reset but used-labels persists
- Two-pass labeling: first reuse existing labels, then assign new ones
- This ensures same positions get same labels across scrolls/navigation."
  ;; Guard against re-entrant calls (prevents overlay→scroll→show-labels loop)
  (unless flash-emacs-search--updating
    (setq flash-emacs-search--updating t)
    (unwind-protect
        (progn
          (flash-emacs-search--clear-overlays)
          (setq flash-emacs-search--all-matches nil)
          (when (and flash-emacs-search--active
                     pattern
                     (>= (length pattern) flash-emacs-search-min-length))
            ;; Initialize labels only when pattern changes (like flash.nvim's M.new)
            (unless (and (equal pattern flash-emacs-search--current-pattern)
                         flash-emacs-search--used-labels)
              (flash-emacs-search--init-labels pattern)
              (setq flash-emacs-search--current-pattern pattern))
            ;; Reset available labels on each update (like flash.nvim's reset)
            ;; but keep used-labels to maintain stable labels
            (let ((filtered-labels (flash-emacs-search--filter-labels flash-emacs-labels pattern)))
              (setq flash-emacs-search--available-labels
                    (mapcar #'char-to-string (string-to-list filtered-labels))))
            ;; Find visible matches and assign labels
            (let* ((win (or (minibuffer-selected-window) (selected-window)))
                   (matches (flash-emacs-search--find-visible-matches pattern))
                   (labeled (flash-emacs-search--assign-labels matches pattern)))
              (setq flash-emacs-search--all-matches labeled)
              ;; Create overlays for all labeled matches
              (dolist (match labeled)
                (let ((label (plist-get match :label)))
                  (when label
                    (let ((ov (flash-emacs-search--create-label-overlay
                               (plist-get match :end-pos)
                               label
                               win)))
                      (when ov
                        (push ov flash-emacs-search--overlays)))))))))
      (setq flash-emacs-search--updating nil))))

;;; Jump handling

(defun flash-emacs-search--find-match-by-label (label)
  "Find match with LABEL in visible matches or used-labels table."
  ;; First check visible matches
  (or (cl-find label flash-emacs-search--all-matches
               :key (lambda (m) (plist-get m :label))
               :test #'string=)
      ;; If not visible, search used-labels for this label
      (when flash-emacs-search--used-labels
        (let ((pos nil))
          (maphash (lambda (k v) (when (string= v label) (setq pos k)))
                   flash-emacs-search--used-labels)
          (when pos
            (let ((win (or (minibuffer-selected-window) (selected-window))))
              (list :pos pos
                    :end-pos (+ pos (length flash-emacs-search--current-pattern))
                    :window win
                    :buffer (window-buffer win)
                    :label label)))))))

(defun flash-emacs-search--jump-to-match (match)
  "Jump to MATCH and exit search."
  ;; Clear everything
  (flash-emacs-search--clear-all)
  (setq flash-emacs-search--active nil)
  ;; Schedule jump before aborting (abort won't trigger exit-hook)
  (let ((target-match match))
    (run-at-time 0 nil
                 (lambda ()
                   (flash-emacs--jump-to-match target-match)
                   (when (fboundp 'evil-ex-delete-hl)
                     (evil-ex-delete-hl 'evil-ex-search)))))
  ;; Abort minibuffer to cancel Evil's search
  (abort-recursive-edit))

;;; Key handling

(defun flash-emacs-search--pre-command ()
  "Intercept label key presses before they're processed."
  (when (and flash-emacs-search--active
             flash-emacs-search--all-matches
             (minibufferp))
    (let* ((keys (this-command-keys-vector))
           (key (and (= (length keys) 1) (aref keys 0)))
           (char-str (and key (characterp key) (char-to-string key)))
           (match (and char-str (flash-emacs-search--find-match-by-label char-str))))
      (when match
        ;; Found a label - jump to it
        (flash-emacs-search--jump-to-match match)
        ;; Cancel the current command
        (setq this-command 'ignore)))))

;;; Toggle command

(defun flash-emacs-search-toggle ()
  "Toggle flash labels during search."
  (interactive)
  (setq flash-emacs-search--active (not flash-emacs-search--active))
  (if flash-emacs-search--active
      (progn
        (let ((pattern (minibuffer-contents-no-properties)))
          (flash-emacs-search--show-labels pattern))
        (message "Flash search: ON"))
    (flash-emacs-search--clear-all)
    (message "Flash search: OFF")))

;;; Evil search integration

(defun flash-emacs-search--update-labels ()
  "Update labels based on current search pattern."
  (when (and flash-emacs-search--active
             (active-minibuffer-window))
    (let ((pattern (with-current-buffer (window-buffer (active-minibuffer-window))
                     (minibuffer-contents-no-properties))))
      (flash-emacs-search--show-labels pattern))))

(defun flash-emacs-search--refresh-labels ()
  "Refresh labels for visible area.
Labels are stable due to used-labels table (flash.nvim's label reuse mechanism).
Reads the current pattern from the minibuffer to stay in sync after jumps."
  (when (and flash-emacs-search--active
             (active-minibuffer-window))
    (let ((pattern (or (with-current-buffer
                           (window-buffer (active-minibuffer-window))
                         (minibuffer-contents-no-properties))
                       flash-emacs-search--current-pattern)))
      (when (and pattern (> (length pattern) 0))
        (flash-emacs-search--show-labels pattern)))))

(defvar flash-emacs-search--update-timer nil
  "Timer for debounced label updates on text change.")

(defun flash-emacs-search--after-change (&rest _)
  "Update labels when search pattern changes (debounced)."
  (when flash-emacs-search--active
    (when flash-emacs-search--update-timer
      (cancel-timer flash-emacs-search--update-timer))
    (setq flash-emacs-search--update-timer
          (run-at-time 0 nil #'flash-emacs-search--update-labels))))

(defvar flash-emacs-search--refresh-timer nil
  "Timer for debounced label refresh.")

(defun flash-emacs-search--post-command ()
  "Refresh labels after each command (debounced).
Like flash.nvim, labels are re-sorted by distance from current cursor."
  (when flash-emacs-search--active
    ;; Debounce to avoid excessive refreshes during rapid commands
    (when flash-emacs-search--refresh-timer
      (cancel-timer flash-emacs-search--refresh-timer))
    (setq flash-emacs-search--refresh-timer
          (run-at-time 0.01 nil #'flash-emacs-search--refresh-labels))))

(defvar flash-emacs-search--scroll-timer nil
  "Timer for debounced scroll updates.")

(defun flash-emacs-search--window-scroll (win _start)
  "Refresh visible labels when window scrolls."
  (when (and flash-emacs-search--active
             (active-minibuffer-window)
             (not (minibufferp (window-buffer win))))
    (when flash-emacs-search--scroll-timer
      (cancel-timer flash-emacs-search--scroll-timer))
    (setq flash-emacs-search--scroll-timer
          (run-at-time 0 nil #'flash-emacs-search--refresh-labels))))

(defun flash-emacs-search--setup ()
  "Set up flash search for Evil ex-search session."
  (when (and (boundp 'evil-ex-search-direction)
             flash-emacs-search-enabled)
    (setq flash-emacs-search--active t
          flash-emacs-search--original-buffer (current-buffer))
    (add-hook 'after-change-functions #'flash-emacs-search--after-change nil t)
    (add-hook 'pre-command-hook #'flash-emacs-search--pre-command nil t)
    (add-hook 'post-command-hook #'flash-emacs-search--post-command nil t)
    (add-hook 'window-scroll-functions #'flash-emacs-search--window-scroll)))

(defun flash-emacs-search--cleanup ()
  "Clean up flash search after search ends."
  (flash-emacs-search--clear-all)
  (when flash-emacs-search--update-timer
    (cancel-timer flash-emacs-search--update-timer))
  (when flash-emacs-search--refresh-timer
    (cancel-timer flash-emacs-search--refresh-timer))
  (when flash-emacs-search--scroll-timer
    (cancel-timer flash-emacs-search--scroll-timer))
  (setq flash-emacs-search--active nil
        flash-emacs-search--update-timer nil
        flash-emacs-search--scroll-timer nil
        flash-emacs-search--refresh-timer nil)
  (remove-hook 'after-change-functions #'flash-emacs-search--after-change t)
  (remove-hook 'pre-command-hook #'flash-emacs-search--pre-command t)
  (remove-hook 'post-command-hook #'flash-emacs-search--post-command t)
  (remove-hook 'window-scroll-functions #'flash-emacs-search--window-scroll))

;;; Keymap for toggle key only

(defun flash-emacs-search--make-toggle-keymap ()
  "Create keymap with toggle key."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd flash-emacs-search-toggle-key) #'flash-emacs-search-toggle)
    map))

;;; Enable / disable

(defvar flash-emacs-search--hooks-installed nil
  "Non-nil when search integration hooks are installed.")

(defun flash-emacs-search-enable ()
  "Enable flash-emacs search integration.
Adds hooks so labels appear during Evil / and ? search."
  (interactive)
  (unless flash-emacs-search--hooks-installed
    (add-hook 'minibuffer-setup-hook #'flash-emacs-search--maybe-setup)
    (add-hook 'minibuffer-exit-hook #'flash-emacs-search--cleanup)
    (setq flash-emacs-search--hooks-installed t)))

(defun flash-emacs-search-disable ()
  "Disable flash-emacs search integration."
  (interactive)
  (when flash-emacs-search--hooks-installed
    (remove-hook 'minibuffer-setup-hook #'flash-emacs-search--maybe-setup)
    (remove-hook 'minibuffer-exit-hook #'flash-emacs-search--cleanup)
    (flash-emacs-search--clear-overlays)
    (setq flash-emacs-search--hooks-installed nil)))

(defun flash-emacs-search--maybe-setup ()
  "Set up flash search if we're in Evil ex-search."
  (when (and flash-emacs-search-enabled
             (boundp 'evil-ex-search-keymap)
             (eq (current-local-map) evil-ex-search-keymap))
    (flash-emacs-search--setup)
    ;; Add toggle key
    (use-local-map (make-composed-keymap
                    (flash-emacs-search--make-toggle-keymap)
                    (current-local-map)))))

;; Auto-enable on load
(flash-emacs-search-enable)

(provide 'flash-emacs-search)

;;; flash-emacs-search.el ends here
