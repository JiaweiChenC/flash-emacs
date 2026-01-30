;;; flash-emacs-ts.el --- Treesitter integration for flash-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Flash-Emacs Contributors
;; Maintainer: Flash-Emacs Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (flash-emacs "1.0.0"))
;; Keywords: navigation, jump, treesitter, convenience
;; URL: https://github.com/flash-emacs/flash-emacs

;;; Commentary:

;; This package provides treesitter integration for flash-emacs,
;; similar to flash.nvim's treesitter functionality.
;;
;; Features:
;; - `flash-emacs-ts-jump': Show labels for all treesitter nodes containing
;;   the cursor (from innermost to outermost), allowing quick selection of
;;   any syntax node.
;;
;; Requirements:
;; - Emacs 29.1 or later (native treesitter support)
;; - A treesitter grammar installed for the current buffer's language
;; - flash-emacs.el

;;; Code:

(require 'treesit)
(require 'flash-emacs)

;;; Customization

(defgroup flash-emacs-ts nil
  "Treesitter integration for flash-emacs."
  :group 'flash-emacs
  :prefix "flash-emacs-ts-")

(defcustom flash-emacs-ts-labels "asdfghjklqwertyuiopzxcvbnm"
  "Characters used as jump labels for treesitter nodes."
  :type 'string
  :group 'flash-emacs-ts)

;;; Faces

(defface flash-emacs-ts-label
  '((t (:background "purple" :foreground "white" :weight bold)))
  "Face for treesitter node labels."
  :group 'flash-emacs-ts)

;;; Internal variables

(defvar flash-emacs-ts--overlays nil
  "List of active treesitter overlays.")

;;; Core functions

(defun flash-emacs-ts--get-parser ()
  "Get or create treesitter parser for current buffer.
Returns nil if treesitter is not available for this buffer."
  (when-let* (((treesit-available-p))
              (lang (treesit-language-at (point))))
    (treesit-parser-create lang)))

(defun flash-emacs-ts--get-nodes-at-point ()
  "Get all treesitter nodes containing point, from innermost to outermost.
Returns a list of nodes, starting with the smallest node at point
and including all parent nodes up to the root.
Deduplicates nodes with identical ranges."
  (when-let* ((_parser (flash-emacs-ts--get-parser))
              (node (treesit-node-at (point))))
    (let ((nodes '())
          (seen-ranges (make-hash-table :test 'equal)))
      (while node
        (let ((range-key (format "%d:%d" 
                                 (treesit-node-start node) 
                                 (treesit-node-end node))))
          (unless (gethash range-key seen-ranges)
            (puthash range-key t seen-ranges)
            (push node nodes)))
        (setq node (treesit-node-parent node)))
      (nreverse nodes))))

(defun flash-emacs-ts--node-to-match (node label)
  "Convert a treesitter NODE to a match structure with LABEL."
  (list :node node
        :pos (treesit-node-start node)
        :end-pos (treesit-node-end node)
        :type (treesit-node-type node)
        :label label
        :window (selected-window)
        :buffer (current-buffer)))

(defun flash-emacs-ts--assign-labels (nodes)
  "Assign labels to NODES and return list of matches."
  (cl-loop for node in nodes
           for label-char in (string-to-list flash-emacs-ts-labels)
           collect (flash-emacs-ts--node-to-match node (char-to-string label-char))))

;;; Overlay management

(defun flash-emacs-ts--make-overlay (pos label-string property)
  "Create a label overlay at POS with LABEL-STRING and PROPERTY tag."
  (let ((ov (make-overlay pos pos)))
    (overlay-put ov 'before-string label-string)
    (overlay-put ov 'flash-emacs-ts property)
    (overlay-put ov 'priority 200)
    ov))

(defun flash-emacs-ts--create-label-overlays (match)
  "Create overlays for MATCH at both start and end positions.
Returns a list of two overlays."
  (when-let* ((label (plist-get match :label))
              (styled-label (propertize label 'face 'flash-emacs-ts-label)))
    (list (flash-emacs-ts--make-overlay (plist-get match :pos) styled-label 'label-start)
          (flash-emacs-ts--make-overlay (plist-get match :end-pos) styled-label 'label-end))))

(defun flash-emacs-ts--show-overlays (matches)
  "Display overlays for all MATCHES.
Each node gets labels at both start and end to show the range."
  (flash-emacs-ts--clear-overlays)
  (dolist (match matches)
    (dolist (ov (flash-emacs-ts--create-label-overlays match))
      (push ov flash-emacs-ts--overlays))))

(defun flash-emacs-ts--clear-overlays ()
  "Remove all treesitter flash overlays."
  (mapc #'delete-overlay flash-emacs-ts--overlays)
  (setq flash-emacs-ts--overlays nil))

;;; Navigation

(defun flash-emacs-ts--find-match-by-label (label matches)
  "Find the match with the given LABEL in MATCHES."
  (cl-find label matches :key (lambda (m) (plist-get m :label)) :test #'string=))

(defun flash-emacs-ts--select-range (match)
  "Select the range of MATCH.
In Evil visual mode, creates a visual selection.
Otherwise, sets the region."
  (let ((start (plist-get match :pos))
        (end (plist-get match :end-pos)))
    (goto-char start)
    (if-let* (((boundp 'evil-state))
              ((fboundp 'evil-visual-make-selection)))
        (evil-visual-make-selection start (1- end) 'char)
      (push-mark end t t))))

;;; Input handling

(defun flash-emacs-ts--handle-input (char current-index matches)
  "Handle input CHAR with CURRENT-INDEX and MATCHES.
Returns (action . value) where action is one of:
  - exit: exit the loop (value is nil for cancel, t for success)
  - nav: navigation (value is new index)
  - nil: no action taken"
  (cond
   ;; ESC or C-g - cancel
   ((memq char '(27 7))
    (message "Flash TS cancelled")
    (cons 'exit nil))
   ;; Enter - select current node
   ((= char 13)
    (flash-emacs-ts--select-range (nth current-index matches))
    (cons 'exit t))
   ;; ; - next (outer) node
   ((= char ?\;)
    (cons 'nav (min (1+ current-index) (1- (length matches)))))
   ;; , - previous (inner) node
   ((= char ?,)
    (cons 'nav (max (1- current-index) 0)))
   ;; Label key - jump to that node
   ((and (>= char 32) (<= char 126))
    (if-let* ((target (flash-emacs-ts--find-match-by-label (char-to-string char) matches)))
        (progn
          (flash-emacs-ts--select-range target)
          (cons 'exit t))
      nil))
   (t nil)))

;;; Main commands

;;;###autoload
(defun flash-emacs-ts-jump ()
  "Start flash treesitter jump mode.
Shows labels for all treesitter nodes containing the cursor,
from innermost to outermost. Press a label key to select that node's range.

Key bindings during jump:
- Label key: Select the corresponding node's range
- ;: Move to next (outer) node
- ,: Move to previous (inner) node
- RET: Select current highlighted node
- ESC/C-g: Cancel"
  (interactive)
  (unless (treesit-available-p)
    (user-error "Treesitter is not available in this Emacs"))
  (unless (flash-emacs-ts--get-parser)
    (user-error "No treesitter parser available for this buffer (language: %s)"
                (or (treesit-language-at (point)) "unknown")))
  (let ((matches (flash-emacs-ts--assign-labels (flash-emacs-ts--get-nodes-at-point))))
    (unless matches
      (user-error "No treesitter nodes found at point"))
    (unwind-protect
        (let ((current-index 0))
          (when flash-emacs-dim-background
            (flash-emacs--dim-windows))
          (flash-emacs-ts--show-overlays matches)
          (catch 'flash-ts-exit
            (while t
              (let* ((current-match (nth current-index matches))
                     (prompt (format "TS [%d/%d] %s: "
                                     (1+ current-index)
                                     (length matches)
                                     (plist-get current-match :type)))
                     (result (flash-emacs-ts--handle-input 
                              (read-char-exclusive prompt)
                              current-index 
                              matches)))
                (pcase result
                  (`(exit . ,_) (throw 'flash-ts-exit (cdr result)))
                  (`(nav . ,new-index) (setq current-index new-index)))))))
      (flash-emacs-ts--clear-overlays)
      (flash-emacs--clear-dim-overlays))))

;;;###autoload
(defun flash-emacs-ts-search ()
  "Combine flash search with treesitter nodes.
First search for text using flash-emacs, then show treesitter
nodes at each match position for more precise selection.

This is similar to flash.nvim's treesitter_search mode."
  (interactive)
  ;; TODO: Implement combined search + treesitter mode
  (user-error "flash-emacs-ts-search is not yet implemented"))

(provide 'flash-emacs-ts)

;;; flash-emacs-ts.el ends here
