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
  (when (treesit-available-p)
    (let ((lang (treesit-language-at (point))))
      (when lang
        (treesit-parser-create lang)))))

(defun flash-emacs-ts--get-nodes-at-point ()
  "Get all treesitter nodes containing point, from innermost to outermost.
Returns a list of nodes, starting with the smallest node at point
and including all parent nodes up to the root.
Deduplicates nodes with identical ranges."
  (when-let* ((parser (flash-emacs-ts--get-parser)))
    (let ((node (treesit-node-at (point)))
          (nodes '())
          (seen-ranges (make-hash-table :test 'equal)))
      ;; Walk up the tree collecting all parent nodes
      (while node
        (let* ((start (treesit-node-start node))
               (end (treesit-node-end node))
               (range-key (format "%d:%d" start end)))
          ;; Only add nodes with unique ranges (deduplicate)
          (unless (gethash range-key seen-ranges)
            (puthash range-key t seen-ranges)
            (push node nodes)))
        (setq node (treesit-node-parent node)))
      ;; Return in order from innermost to outermost
      (nreverse nodes))))

(defun flash-emacs-ts--node-to-match (node label)
  "Convert a treesitter NODE to a match structure with LABEL."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node))
        (type (treesit-node-type node)))
    (list :node node
          :pos start
          :end-pos end
          :type type
          :label label
          :window (selected-window)
          :buffer (current-buffer))))

(defun flash-emacs-ts--assign-labels (nodes)
  "Assign labels to NODES and return list of matches."
  (let ((labels (string-to-list flash-emacs-ts-labels))
        (matches '()))
    (cl-loop for node in nodes
             for label-char in labels
             do (push (flash-emacs-ts--node-to-match 
                       node 
                       (char-to-string label-char))
                      matches))
    (nreverse matches)))

;;; Overlay management

(defun flash-emacs-ts--create-label-overlays (match)
  "Create overlays for the label of MATCH at both start and end positions.
Returns a list of overlays (start-overlay end-overlay)."
  (let* ((start-pos (plist-get match :pos))
         (end-pos (plist-get match :end-pos))
         (label (plist-get match :label))
         (styled-label (propertize label 'face 'flash-emacs-ts-label))
         (overlays '()))
    (when label
      ;; Create overlay at start of node (before-string)
      (let ((start-ov (make-overlay start-pos start-pos)))
        (overlay-put start-ov 'before-string styled-label)
        (overlay-put start-ov 'flash-emacs-ts 'label-start)
        (overlay-put start-ov 'priority 200)
        (push start-ov overlays))
      ;; Create overlay at end of node (after-string)
      (let ((end-ov (make-overlay end-pos end-pos)))
        (overlay-put end-ov 'before-string styled-label)
        (overlay-put end-ov 'flash-emacs-ts 'label-end)
        (overlay-put end-ov 'priority 200)
        (push end-ov overlays)))
    overlays))

(defun flash-emacs-ts--show-overlays (matches)
  "Display overlays for all MATCHES.
Each node gets labels at both start and end to show the range."
  (flash-emacs-ts--clear-overlays)
  (dolist (match matches)
    (dolist (ov (flash-emacs-ts--create-label-overlays match))
      (push ov flash-emacs-ts--overlays))))

(defun flash-emacs-ts--clear-overlays ()
  "Remove all treesitter flash overlays."
  (dolist (overlay flash-emacs-ts--overlays)
    (delete-overlay overlay))
  (setq flash-emacs-ts--overlays nil))

;;; Navigation

(defun flash-emacs-ts--find-match-by-label (label matches)
  "Find the match with the given LABEL in MATCHES."
  (cl-find-if (lambda (match)
                (string= (plist-get match :label) label))
              matches))

(defun flash-emacs-ts--select-range (match)
  "Select the range of MATCH.
In Evil visual mode, creates a visual selection.
Otherwise, sets the region."
  (let ((start (plist-get match :pos))
        (end (plist-get match :end-pos)))
    ;; Handle Evil visual mode if available
    (if (and (boundp 'evil-state)
             (fboundp 'evil-visual-make-selection))
        (progn
          (goto-char start)
          (evil-visual-make-selection start (1- end) 'char))
      ;; Standard Emacs region
      (goto-char start)
      (push-mark end t t))))

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
  
  (let* ((nodes (flash-emacs-ts--get-nodes-at-point))
         (matches (flash-emacs-ts--assign-labels nodes))
         (current-index 0)
         (current-match (nth current-index matches)))
    
    (unless matches
      (user-error "No treesitter nodes found at point"))
    
    (unwind-protect
        (catch 'flash-ts-exit
          ;; Apply dimming if enabled
          (when flash-emacs-dim-background
            (flash-emacs--dim-windows))
          
          ;; Show initial overlays
          (flash-emacs-ts--show-overlays matches)
          
          (while t
            (let* ((prompt (format "TS [%d/%d] %s: "
                                   (1+ current-index)
                                   (length matches)
                                   (plist-get current-match :type)))
                   (char (read-char-exclusive prompt)))
              
              (cond
               ;; ESC or C-g - exit
               ((or (= char 27) (= char 7))
                (message "Flash TS cancelled")
                (throw 'flash-ts-exit nil))
               
               ;; Enter - select current node
               ((= char 13)
                (flash-emacs-ts--select-range current-match)
                (throw 'flash-ts-exit t))
               
               ;; ; - next (outer) node
               ((= char ?\;)
                (when (< current-index (1- (length matches)))
                  (setq current-index (1+ current-index))
                  (setq current-match (nth current-index matches))))
               
               ;; , - previous (inner) node
               ((= char ?,)
                (when (> current-index 0)
                  (setq current-index (1- current-index))
                  (setq current-match (nth current-index matches))))
               
               ;; Check if it's a label
               ((and (>= char 32) (<= char 126))
                (let ((target-match (flash-emacs-ts--find-match-by-label 
                                     (char-to-string char) 
                                     matches)))
                  (when target-match
                    (flash-emacs-ts--select-range target-match)
                    (throw 'flash-ts-exit t))))
               
               ;; Unknown - ignore
               (t nil)))))
      
      ;; Cleanup
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
  ;; This would require deeper integration with flash-emacs-jump
  (user-error "flash-emacs-ts-search is not yet implemented"))

(provide 'flash-emacs-ts)

;;; flash-emacs-ts.el ends here
