;;; flash-emacs-search.el --- Search integration for flash-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Flash-Emacs Contributors
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
;;   (flash-emacs-search-mode 1)
;;
;; Then use Evil's / or ? to search. Labels will appear automatically.
;; Press a label key to jump to that match and exit search.
;;
;; Toggle with C-s during search to enable/disable labels.

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

(defvar flash-emacs-search--matches nil
  "Current labeled matches.")

(defvar flash-emacs-search--original-buffer nil
  "Buffer where search started.")

;;; Overlay management

(defun flash-emacs-search--clear-overlays ()
  "Clear all flash search overlays."
  (mapc #'delete-overlay flash-emacs-search--overlays)
  (setq flash-emacs-search--overlays nil
        flash-emacs-search--matches nil))

(defun flash-emacs-search--create-label-overlay (end-pos label win)
  "Create a label overlay at END-POS (after match) with LABEL in window WIN.
Uses replace style - the label replaces the character after the match."
  (with-selected-window win
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
      ov)))

;;; Window helpers

(defun flash-emacs-search--get-windows ()
  "Get windows for search, excluding minibuffer."
  (cl-remove-if (lambda (win)
                  (or (minibufferp (window-buffer win))
                      (not (window-live-p win))))
                (flash-emacs--get-windows)))

;;; Match finding

(defun flash-emacs-search--find-matches (pattern)
  "Find all visible matches for PATTERN and return as match list.
PATTERN is treated as a literal string for searching."
  (when (and pattern (>= (length pattern) flash-emacs-search-min-length))
    (let ((matches '())
          ;; Use the same case-sensitivity as flash-emacs
          (case-fold-search (flash-emacs--should-ignore-case pattern))
          ;; Quote the pattern for literal matching
          (search-regexp (regexp-quote pattern)))
      ;; Only search in non-minibuffer windows
      (dolist (win (flash-emacs-search--get-windows))
        (with-selected-window win
          (save-excursion
            (goto-char (window-start))
            (let ((limit (window-end nil t)))
              (condition-case nil
                  (while (re-search-forward search-regexp limit t)
                    (let ((start (match-beginning 0))
                          (end (match-end 0)))
                      (unless (= start end)
                        (push (list :pos start
                                    :end-pos end
                                    :window win
                                    :buffer (current-buffer))
                              matches))))
                (invalid-regexp nil))))))
      (nreverse matches))))

;;; Label assignment

(defun flash-emacs-search--find-buffer-conflicts (pattern labels)
  "Find labels that conflict with PATTERN continuation in the entire buffer.
Since search wraps around, we need to check the whole buffer, not just visible."
  (when (and pattern (> (length pattern) 0))
    (let* ((search-regexp (concat (regexp-quote pattern) "."))
           (case-fold-search (flash-emacs--should-ignore-case pattern))
           (conflicts '()))
      ;; Check in all non-minibuffer windows' buffers
      (dolist (win (flash-emacs-search--get-windows))
        (with-current-buffer (window-buffer win)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward search-regexp nil t)
              (let ((following-char (buffer-substring-no-properties
                                     (1- (match-end 0)) (match-end 0))))
                (dolist (label labels)
                  (when (if case-fold-search
                            (string= (downcase following-char) (downcase label))
                          (string= following-char label))
                    (push label conflicts))))))))
      (delete-dups conflicts))))

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

(defun flash-emacs-search--assign-labels (matches pattern)
  "Assign labels to MATCHES, avoiding conflicts with PATTERN continuation.

For search, we check the ENTIRE buffer for conflicts (not just visible)
since search can wrap around."
  (let* ((current-window (or (minibuffer-selected-window) (selected-window)))
         (current-point (with-selected-window current-window (point)))
         ;; Filter labels against entire buffer
         (filtered-labels (flash-emacs-search--filter-labels flash-emacs-labels pattern))
         (sorted-matches (flash-emacs--sort-matches matches current-point current-window))
         (available (mapcar #'char-to-string (string-to-list filtered-labels)))
         (labeled '()))
    ;; Simple assignment - no reuse needed for search
    (dolist (match sorted-matches)
      (when available
        (let ((label (pop available)))
          (plist-put match :label label)
          (push match labeled))))
    (nreverse labeled)))

;;; Display

(defun flash-emacs-search--show-labels (pattern)
  "Show flash labels for PATTERN matches."
  (flash-emacs-search--clear-overlays)
  (when (and flash-emacs-search--active
             pattern
             (>= (length pattern) flash-emacs-search-min-length))
    (let* ((matches (flash-emacs-search--find-matches pattern))
           (labeled (flash-emacs-search--assign-labels matches pattern)))
      (setq flash-emacs-search--matches labeled)
      (dolist (match labeled)
        (when-let* ((label (plist-get match :label))
                    (win (plist-get match :window))
                    ;; Don't show in minibuffer
                    ((not (minibufferp (window-buffer win)))))
          ;; Place label AFTER the match (at end-pos)
          (let ((ov (flash-emacs-search--create-label-overlay
                     (plist-get match :end-pos)
                     label
                     win)))
            (push ov flash-emacs-search--overlays)))))))

;;; Jump handling

(defun flash-emacs-search--find-match-by-label (label)
  "Find match with LABEL in current matches."
  (cl-find label flash-emacs-search--matches
           :key (lambda (m) (plist-get m :label))
           :test #'string=))

(defun flash-emacs-search--jump-to-match (match)
  "Jump to MATCH and exit search."
  ;; Clear overlays first
  (flash-emacs-search--clear-overlays)
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
             flash-emacs-search--matches
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
    (flash-emacs-search--clear-overlays)
    (message "Flash search: OFF")))

;;; Evil search integration

(defun flash-emacs-search--update-labels ()
  "Update labels based on current pattern and visible matches."
  (when flash-emacs-search--active
    (let ((pattern (minibuffer-contents-no-properties)))
      (flash-emacs-search--show-labels pattern))))

(defun flash-emacs-search--after-change (&rest _)
  "Update labels after search pattern changes."
  (when flash-emacs-search--active
    ;; Use timer to avoid issues during minibuffer changes
    (run-at-time 0 nil #'flash-emacs-search--update-labels)))

(defun flash-emacs-search--post-command ()
  "Update labels after each command (handles wrap, scroll, etc)."
  (when flash-emacs-search--active
    ;; Refresh labels to reflect current visible matches
    (flash-emacs-search--update-labels)))

(defvar flash-emacs-search--scroll-timer nil
  "Timer for debounced scroll updates.")

(defun flash-emacs-search--advice-update (&rest _)
  "Advice to update labels immediately after Evil search updates."
  (when flash-emacs-search--active
    ;; Schedule update for next event loop iteration
    ;; This ensures Evil has finished its update
    (run-at-time 0 nil #'flash-emacs-search--update-labels)))

(defun flash-emacs-search--window-scroll (win _start)
  "Update labels when window scrolls (e.g., during search wrap).
WIN is the window that scrolled."
  (when (and flash-emacs-search--active
             (active-minibuffer-window)
             ;; Only react to main buffer windows, not minibuffer
             (not (minibufferp (window-buffer win))))
    ;; Debounce rapid scroll events
    (when flash-emacs-search--scroll-timer
      (cancel-timer flash-emacs-search--scroll-timer))
    (setq flash-emacs-search--scroll-timer
          (run-at-time 0.02 nil #'flash-emacs-search--update-labels))))

(defun flash-emacs-search--setup ()
  "Set up flash search for Evil ex-search session."
  (when (and (boundp 'evil-ex-search-direction)
             flash-emacs-search-enabled)
    (setq flash-emacs-search--active t
          flash-emacs-search--original-buffer (current-buffer))
    (add-hook 'after-change-functions #'flash-emacs-search--after-change nil t)
    (add-hook 'pre-command-hook #'flash-emacs-search--pre-command nil t)
    (add-hook 'post-command-hook #'flash-emacs-search--post-command nil t)
    ;; Hook window scroll for immediate updates on wrap/scroll
    (add-hook 'window-scroll-functions #'flash-emacs-search--window-scroll)
    ;; Advice Evil's search update for immediate label refresh on wrap
    (when (fboundp 'evil-ex-search-update-pattern)
      (advice-add 'evil-ex-search-update-pattern :after #'flash-emacs-search--advice-update))))

(defun flash-emacs-search--cleanup ()
  "Clean up flash search after search ends."
  (flash-emacs-search--clear-overlays)
  (setq flash-emacs-search--active nil
        flash-emacs-search--scroll-timer nil)
  (remove-hook 'after-change-functions #'flash-emacs-search--after-change t)
  (remove-hook 'pre-command-hook #'flash-emacs-search--pre-command t)
  (remove-hook 'post-command-hook #'flash-emacs-search--post-command t)
  (remove-hook 'window-scroll-functions #'flash-emacs-search--window-scroll)
  (when (fboundp 'evil-ex-search-update-pattern)
    (advice-remove 'evil-ex-search-update-pattern #'flash-emacs-search--advice-update)))

;;; Keymap for toggle key only

(defun flash-emacs-search--make-toggle-keymap ()
  "Create keymap with toggle key."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd flash-emacs-search-toggle-key) #'flash-emacs-search-toggle)
    map))

;;; Minor mode

;;;###autoload
(define-minor-mode flash-emacs-search-mode
  "Minor mode to integrate flash-emacs labels with Evil search.
When enabled, labels appear next to search matches during / and ? search,
allowing you to jump directly to any match by pressing its label."
  :global t
  :lighter " FlashS"
  (if flash-emacs-search-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'flash-emacs-search--maybe-setup)
        (add-hook 'minibuffer-exit-hook #'flash-emacs-search--cleanup))
    (remove-hook 'minibuffer-setup-hook #'flash-emacs-search--maybe-setup)
    (remove-hook 'minibuffer-exit-hook #'flash-emacs-search--cleanup)
    (flash-emacs-search--clear-overlays)))

(defun flash-emacs-search--maybe-setup ()
  "Set up flash search if we're in Evil ex-search."
  (when (and (boundp 'evil-ex-search-keymap)
             (eq (current-local-map) evil-ex-search-keymap))
    (flash-emacs-search--setup)
    ;; Add toggle key
    (use-local-map (make-composed-keymap
                    (flash-emacs-search--make-toggle-keymap)
                    (current-local-map)))))

(provide 'flash-emacs-search)

;;; flash-emacs-search.el ends here
