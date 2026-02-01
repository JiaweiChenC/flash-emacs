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

;;; Customization - Rainbow

(defcustom flash-emacs-ts-rainbow-enabled nil
  "Whether to use rainbow colors for treesitter labels.
When enabled, labels at different depths have different colors."
  :type 'boolean
  :group 'flash-emacs-ts)

;;; Faces

(defface flash-emacs-ts-label
  '((t (:background "red" :foreground "white" :weight bold)))
  "Face for treesitter node labels."
  :group 'flash-emacs-ts)

;; Rainbow faces for different depths (inspired by flash.nvim)
(defface flash-emacs-ts-rainbow-1
  '((t (:background "#ff007c" :foreground "white" :weight bold)))
  "Rainbow face for depth 1 (innermost)."
  :group 'flash-emacs-ts)

(defface flash-emacs-ts-rainbow-2
  '((t (:background "#00dfff" :foreground "black" :weight bold)))
  "Rainbow face for depth 2."
  :group 'flash-emacs-ts)

(defface flash-emacs-ts-rainbow-3
  '((t (:background "#ff9e64" :foreground "black" :weight bold)))
  "Rainbow face for depth 3."
  :group 'flash-emacs-ts)

(defface flash-emacs-ts-rainbow-4
  '((t (:background "#bb9af7" :foreground "black" :weight bold)))
  "Rainbow face for depth 4."
  :group 'flash-emacs-ts)

(defface flash-emacs-ts-rainbow-5
  '((t (:background "#9ece6a" :foreground "black" :weight bold)))
  "Rainbow face for depth 5."
  :group 'flash-emacs-ts)

(defface flash-emacs-ts-rainbow-6
  '((t (:background "#e0af68" :foreground "black" :weight bold)))
  "Rainbow face for depth 6."
  :group 'flash-emacs-ts)

(defface flash-emacs-ts-rainbow-7
  '((t (:background "#7dcfff" :foreground "black" :weight bold)))
  "Rainbow face for depth 7."
  :group 'flash-emacs-ts)

(defface flash-emacs-ts-rainbow-8
  '((t (:background "#f7768e" :foreground "black" :weight bold)))
  "Rainbow face for depth 8."
  :group 'flash-emacs-ts)

(defface flash-emacs-ts-rainbow-9
  '((t (:background "#73daca" :foreground "black" :weight bold)))
  "Rainbow face for depth 9 (outermost)."
  :group 'flash-emacs-ts)

(defvar flash-emacs-ts-rainbow-faces
  '(flash-emacs-ts-rainbow-1
    flash-emacs-ts-rainbow-2
    flash-emacs-ts-rainbow-3
    flash-emacs-ts-rainbow-4
    flash-emacs-ts-rainbow-5
    flash-emacs-ts-rainbow-6
    flash-emacs-ts-rainbow-7
    flash-emacs-ts-rainbow-8
    flash-emacs-ts-rainbow-9)
  "List of faces for rainbow labels, from innermost to outermost.")

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

(defun flash-emacs-ts--node-to-match (node label depth)
  "Convert a treesitter NODE to a match structure with LABEL and DEPTH."
  (list :node node
        :pos (treesit-node-start node)
        :end-pos (treesit-node-end node)
        :type (treesit-node-type node)
        :label label
        :depth depth
        :window (selected-window)
        :buffer (current-buffer)))

(defun flash-emacs-ts--assign-labels (nodes)
  "Assign labels to NODES and return list of matches.
Includes depth information for rainbow coloring."
  (cl-loop for node in nodes
           for label-char in (string-to-list flash-emacs-ts-labels)
           for depth from 0
           collect (flash-emacs-ts--node-to-match node (char-to-string label-char) depth)))

(defun flash-emacs-ts--get-label-face (depth)
  "Get the face for a label at DEPTH.
If rainbow is enabled, returns a rainbow face based on depth.
Otherwise returns the default label face."
  (if flash-emacs-ts-rainbow-enabled
      (let ((faces flash-emacs-ts-rainbow-faces)
            (idx (mod depth (length flash-emacs-ts-rainbow-faces))))
        (nth idx faces))
    'flash-emacs-ts-label))

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
Returns a list of two overlays.
Uses rainbow colors if enabled."
  (when-let* ((label (plist-get match :label))
              (depth (or (plist-get match :depth) 0))
              (face (flash-emacs-ts--get-label-face depth))
              (styled-label (propertize label 'face face)))
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

;; Declare Evil variables to avoid byte-compiler warnings
(defvar evil-state)
(defvar evil-this-operator)
(defvar evil-inhibit-operator)
(defvar evil-operator-state-map)
(defvar evil-normal-state-map)
(declare-function evil-define-motion "evil-macros")

;; Forward declarations for flash-emacs-remote functions
(declare-function flash-emacs-remote--save-state "flash-emacs-remote")
(declare-function flash-emacs-remote--restore-state "flash-emacs-remote")
(declare-function flash-emacs-remote--start-waiting "flash-emacs-remote")
(defvar flash-emacs-remote-restore)

(defun flash-emacs-ts--select-range (match)
  "Select the range of MATCH.
If Evil is available, creates a visual selection.
Otherwise, sets the Emacs region."
  (let ((start (plist-get match :pos))
        (end (plist-get match :end-pos)))
    (goto-char start)
    (if (fboundp 'evil-visual-make-selection)
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
              (let ((result (flash-emacs-ts--handle-input 
                             (read-char-exclusive (concat (flash-emacs-icon) "TS: "))
                             current-index 
                             matches)))
                (pcase result
                  (`(exit . ,_) (throw 'flash-ts-exit (cdr result)))
                  (`(nav . ,new-index) (setq current-index new-index)))))))
      (flash-emacs-ts--clear-overlays)
      (flash-emacs--clear-dim-overlays))))

;;; Internal function for treesitter selection (returns match or nil)

(defun flash-emacs-ts--handle-input-for-operator (char current-index matches)
  "Handle input CHAR for operator mode, returning the selected match directly.
Returns (action . value) where:
  - (exit . nil): cancelled
  - (exit . match): selected match
  - (nav . index): navigation to new index"
  (cond
   ;; ESC or C-g - cancel
   ((memq char '(27 7))
    (cons 'exit nil))
   ;; Enter - select current node
   ((= char 13)
    (cons 'exit (nth current-index matches)))
   ;; ; - next (outer) node
   ((= char ?\;)
    (cons 'nav (min (1+ current-index) (1- (length matches)))))
   ;; , - previous (inner) node
   ((= char ?,)
    (cons 'nav (max (1- current-index) 0)))
   ;; Label key - select that node
   ((and (>= char 32) (<= char 126))
    (if-let* ((target (flash-emacs-ts--find-match-by-label (char-to-string char) matches)))
        (cons 'exit target)
      nil))
   (t nil)))

(defun flash-emacs-ts--do-selection ()
  "Show treesitter nodes at point and let user select one.
Returns the selected match or nil if cancelled."
  (let ((matches (flash-emacs-ts--assign-labels (flash-emacs-ts--get-nodes-at-point))))
    (when matches
      (unwind-protect
          (let ((current-index 0))
            (when flash-emacs-dim-background
              (flash-emacs--dim-windows))
            (flash-emacs-ts--show-overlays matches)
            (catch 'flash-ts-exit
              (while t
                (let ((result (flash-emacs-ts--handle-input-for-operator
                               (read-char-exclusive (concat (flash-emacs-icon) "TS: "))
                               current-index
                               matches)))
                  (pcase result
                    (`(exit . ,match) (throw 'flash-ts-exit match))
                    (`(nav . ,new-index) (setq current-index new-index)))))))
        (flash-emacs-ts--clear-overlays)
        (flash-emacs--clear-dim-overlays)))))

;;; Evil operator integration (yS, dS, cS)

(defcustom flash-emacs-ts-key "S"
  "Key to trigger treesitter selection in normal and operator-pending mode."
  :type 'string
  :group 'flash-emacs-ts)

(defvar evil-goggles-duration)
(declare-function evil-goggles--show-p "evil-goggles")
(declare-function evil-goggles--get-face "evil-goggles")

(defun flash-emacs-ts--blink-range (start end &optional op)
  "Briefly highlight the range from START to END.
If evil-goggles is available, use it. Otherwise use pulse.
OP is the operator being applied, used to get the correct face."
  (cond
   ;; Try evil-goggles if available and enabled
   ((and (bound-and-true-p evil-goggles-mode)
         (fboundp 'evil-goggles--show-p)
         (fboundp 'evil-goggles--get-face))
    (let* ((face (or (and op (evil-goggles--get-face op))
                     'evil-goggles-default-face
                     'region))
           (ov (make-overlay start end)))
      (overlay-put ov 'face face)
      (overlay-put ov 'priority 9999)
      (redisplay)
      (sit-for (or evil-goggles-duration 0.2))
      (delete-overlay ov)))
   ;; Fallback to pulse
   (t
    (require 'pulse)
    (let ((pulse-iterations 10)
          (pulse-delay 0.03))
      (pulse-momentary-highlight-region start end 'flash-emacs-match)))))

(with-eval-after-load 'evil
  (require 'evil-macros)
  (require 'flash-emacs-remote nil t)  ; For state management

  (evil-define-motion flash-emacs-ts-motion (count)
    "Select a treesitter node at cursor and apply operator.
Use as: yS to yank a treesitter node, dS to delete, cS to change."
    :type inclusive
    :jump t
    (interactive "<c>")
    (unless (treesit-available-p)
      (user-error "Treesitter is not available"))
    (unless (flash-emacs-ts--get-parser)
      (user-error "No treesitter parser for this buffer"))
    (let ((op evil-this-operator)
          (start-pos (point))
          (start-win (selected-window))
          (start-buf (current-buffer)))
      (if (not op)
          ;; No operator - just do visual selection (S in normal mode)
          (flash-emacs-ts-jump)
        ;; In operator mode - apply operator to selected node
        ;; Abort current operator - we'll apply it directly
        (setq evil-inhibit-operator t)
        ;; Do treesitter selection
        (let ((match (flash-emacs-ts--do-selection)))
          (when match
            ;; Got a match - apply operator to its range
            (let ((ts-start (plist-get match :pos))
                  (ts-end (plist-get match :end-pos)))
              ;; Blink the range to show what will be operated on
              (flash-emacs-ts--blink-range ts-start ts-end op)
              ;; Apply operator to treesitter node range
              (funcall op ts-start ts-end)
              ;; Return cursor to original position (for yank)
              ;; For delete/change, cursor should stay at ts-start
              (when (eq op 'evil-yank)
                (goto-char start-pos))))))))

  ;; Bind in operator-pending mode
  (define-key evil-operator-state-map
              (kbd flash-emacs-ts-key)
              #'flash-emacs-ts-motion)

  ;; Also bind in normal mode for visual selection
  (define-key evil-normal-state-map
              (kbd flash-emacs-ts-key)
              #'flash-emacs-ts-jump))

(provide 'flash-emacs-ts)

;;; flash-emacs-ts.el ends here
