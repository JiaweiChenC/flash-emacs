;;; flash-emacs-remote.el --- Remote operations for flash-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Flash-Emacs Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (flash-emacs "1.0.0") (evil "1.0.0"))
;; Keywords: convenience
;; URL: https://github.com/flash-emacs/flash-emacs

;;; Commentary:

;; Remote operations for flash-emacs, inspired by flash.nvim.
;;
;; Usage: ys<search><label>iw - yank inner word at remote location
;;
;; How it works:
;; 1. `y` - enters operator-pending mode
;; 2. `s` - triggers flash jump, moves cursor to target
;; 3. `iw` - text object at target location
;; 4. Yank applies to that text object
;; 5. Cursor returns to original position
;;
;; Setup:
;;   (require 'flash-emacs-remote)

;;; Code:

(require 'flash-emacs)

(eval-when-compile
  (require 'evil nil t))

;;; Customization

(defgroup flash-emacs-remote nil
  "Remote operation support for flash-emacs."
  :group 'flash-emacs
  :prefix "flash-emacs-remote-")

(defcustom flash-emacs-remote-key "r"
  "Key to trigger remote operation in operator-pending mode."
  :type 'string
  :group 'flash-emacs-remote)

(defcustom flash-emacs-remote-restore t
  "Whether to restore cursor position after remote operation."
  :type 'boolean
  :group 'flash-emacs-remote)

;;; Variables for Evil compatibility

(defvar evil-this-operator)
(defvar evil-this-register)
(defvar evil-state)
(defvar evil-inhibit-operator)
(defvar evil-operator-state-map)
(defvar evil-motion-state-map)
(defvar evil-normal-state-map)
(defvar flash-emacs--remote-operation)
(declare-function evil-yank "evil-commands")
(declare-function evil-delete "evil-commands")
(declare-function evil-change "evil-commands")
(declare-function evil-upcase "evil-commands")
(declare-function evil-downcase "evil-commands")
(declare-function evil-indent "evil-commands")
(declare-function evil-shift-left "evil-commands")
(declare-function evil-shift-right "evil-commands")
(declare-function evil-invert-char "evil-commands")
(declare-function evil-rot13 "evil-commands")
(declare-function evil-fill "evil-commands")
(declare-function evil-fill-and-move "evil-commands")
(declare-function evil-define-motion "evil-macros")
(declare-function evil-normal-state "evil-states")
(declare-function evil-insert-state "evil-states")
(declare-function evil-insert "evil-commands")
(declare-function evil-with-state "evil-macros")
(defvar evil-insert-state-exit-hook)

;;; Internal state

(defvar flash-emacs-remote--saved-state nil
  "Saved state: (window point win-start buffer).")

(defvar flash-emacs-remote--waiting nil
  "Non-nil when waiting for second operator to complete.")

;;; State management

(defun flash-emacs-remote--save-state ()
  "Save current state for later restoration."
  (setq flash-emacs-remote--saved-state
        (list (selected-window) (point) (window-start) (current-buffer))))

(defun flash-emacs-remote--restore-state ()
  "Restore saved state."
  (when-let* ((state flash-emacs-remote--saved-state)
              (win (nth 0 state))
              (pt (nth 1 state))
              (ws (nth 2 state))
              (buf (nth 3 state)))
    (when (buffer-live-p buf)
      ;; Only switch window/buffer if we're in a different buffer
      (unless (eq buf (current-buffer))
        (when (window-live-p win)
          (select-window win)
          (set-window-start win ws))
        (unless (eq (current-buffer) buf)
          (set-window-buffer (selected-window) buf)))
      (goto-char pt)))
  (setq flash-emacs-remote--saved-state nil
        flash-emacs-remote--waiting nil))

;;; Post-command hook for restoration

(defun flash-emacs-remote--post-command ()
  "Restore after the re-invoked operator completes.
For change operator, waits until exiting insert mode."
  (when flash-emacs-remote--waiting
    ;; Wait until we're in normal state
    ;; This handles: y/d (operator→normal), c (operator→insert→normal)
    (when (and (boundp 'evil-state)
               (eq evil-state 'normal))
      (remove-hook 'post-command-hook #'flash-emacs-remote--post-command)
      (when flash-emacs-remote-restore
        (run-at-time 0 nil #'flash-emacs-remote--restore-state)))))

(defun flash-emacs-remote--start-waiting ()
  "Start waiting for operator completion. Called after motion setup."
  (setq flash-emacs-remote--waiting t)
  (add-hook 'post-command-hook #'flash-emacs-remote--post-command))

;;; Main implementation

(with-eval-after-load 'evil
  (require 'evil-macros)
  (require 'evil-commands)
  
  (defvar flash-emacs-remote--debug t
    "When non-nil, enable debug logging.")
  
  (defun flash-emacs-remote--read-text-object-or-motion ()
    "Read a text object or motion from user input.
Returns (beg end type) list or nil if cancelled."
    (let ((keys (vector))
          (done nil)
          range)
      ;; Read keys until we get a valid text object/motion or user cancels
      (while (not done)
        (let* ((key (read-event "Motion/text-object: "))
               (key-vec (vector key)))
          (setq keys (vconcat keys key-vec))
          ;; Try to find binding in operator-pending state map first,
          ;; then normal state map
          (let ((binding (or (lookup-key evil-operator-state-map keys)
                             (lookup-key evil-motion-state-map keys)
                             (lookup-key evil-normal-state-map keys))))
            (cond
             ;; ESC or C-g cancels
             ((or (eq key 27) (eq key 7))
              (setq done t range nil))
             ;; Found a complete binding (not a keymap prefix)
             ((and binding (not (keymapp binding)) (commandp binding))
              ;; Save point to detect motion
              (let ((start-pos (point)))
                ;; Call the text-object/motion in operator state context
                (condition-case err
                    (let ((result (evil-with-state operator
                                    (call-interactively binding))))
                      (cond
                       ;; Text objects return (beg end type [properties...])
                       ((and (listp result)
                             (>= (length result) 2)
                             (numberp (car result))
                             (numberp (cadr result)))
                        (setq range result))
                       ;; Motion: use start-pos to current point
                       ((not (= (point) start-pos))
                        (setq range (list (min start-pos (point))
                                          (max start-pos (point))
                                          'inclusive)))
                       ;; No range detected
                       (t (setq range nil))))
                  (error
                   (when flash-emacs-remote--debug
                     (message "flash-remote: error executing %S: %S" binding err))
                   (setq range nil))))
              (setq done t))
             ;; Prefix key - continue reading
             ((keymapp binding)
              nil)
             ;; Number: could be count prefix
             ((and (characterp key) (>= key ?0) (<= key ?9))
              nil)
             ;; No binding found
             ((not binding)
              (message "flash-remote: no binding for %s" (key-description keys))
              (setq done t range nil))))))
      range))
  
  (defun flash-emacs-remote--setup-insert-exit-hook ()
    "Set up hook to restore position when exiting insert mode."
    (when flash-emacs-remote--debug
      (message "flash-remote: setting up insert exit hook"))
    (add-hook 'evil-insert-state-exit-hook
              #'flash-emacs-remote--on-insert-exit nil t))
  
  (defun flash-emacs-remote--on-insert-exit ()
    "Called when exiting insert state after change operation."
    (remove-hook 'evil-insert-state-exit-hook
                 #'flash-emacs-remote--on-insert-exit t)
    (when flash-emacs-remote--debug
      (message "flash-remote: insert exit, restoring position"))
    (when flash-emacs-remote-restore
      ;; Small delay to let the state transition complete
      (run-at-time 0.01 nil #'flash-emacs-remote--restore-state)))
  
  (defun flash-emacs-remote--apply-operator (op range &optional register)
    "Apply operator OP on RANGE (beg end type) with REGISTER.
Returns t if the operator enters insert mode (change), nil otherwise."
    (let* ((beg (car range))
           (end (cadr range))
           (type (or (caddr range) 'inclusive))
           (enters-insert nil))
      (when flash-emacs-remote--debug
        (message "flash-remote: applying %S on %d-%d type=%S reg=%S"
                 op beg end type register))
      (let ((evil-this-register register))
        (condition-case err
            (cond
             ;; Yank - handles register
             ((memq op '(evil-yank evil-org-yank))
              (evil-yank beg end type register))
             ;; Delete - handles register
             ((memq op '(evil-delete evil-org-delete))
              (evil-delete beg end type register))
             ;; Change - handles register, enters insert mode
             ((memq op '(evil-change evil-org-change))
              (setq enters-insert t)
              (when flash-emacs-remote--debug
                (message "flash-remote: CHANGE op detected, range %d-%d" beg end))
              ;; Set up hook to restore after insert exit
              (flash-emacs-remote--setup-insert-exit-hook)
              ;; Delete the text first
              (delete-region beg end)
              (goto-char beg)
              (when flash-emacs-remote--debug
                (message "flash-remote: deleted region, now at %d" (point)))
              ;; Schedule insert state entry after motion completes
              ;; This avoids Evil resetting state when motion returns
              (run-at-time 0 nil
                           (lambda ()
                             (when flash-emacs-remote--debug
                               (message "flash-remote: timer fired, entering insert"))
                             (evil-insert-state))))
             ;; Upcase
             ((eq op 'evil-upcase)
              (evil-upcase beg end type))
             ;; Downcase
             ((eq op 'evil-downcase)
              (evil-downcase beg end type))
             ;; Indent
             ((eq op 'evil-indent)
              (evil-indent beg end))
             ;; Shift
             ((eq op 'evil-shift-left)
              (evil-shift-left beg end))
             ((eq op 'evil-shift-right)
              (evil-shift-right beg end))
             ;; Invert case
             ((eq op 'evil-invert-char)
              (evil-invert-char beg end type))
             ;; Rot13
             ((eq op 'evil-rot13)
              (evil-rot13 beg end type))
             ;; Fill/format
             ((eq op 'evil-fill)
              (evil-fill beg end))
             ((eq op 'evil-fill-and-move)
              (evil-fill-and-move beg end))
             ;; Default: try calling with beg/end/type
             (t
              (condition-case nil
                  (funcall op beg end type)
                (wrong-number-of-arguments
                 (funcall op beg end)))))
          (error
           (message "flash-remote: error applying %S: %S" op err))))
      enters-insert))
  
  (defun flash-emacs-remote--log-position (tag)
    "Log current position with TAG for debugging."
    (when flash-emacs-remote--debug
      (message "flash-remote [%s]: win=%S buf=%S pos=%d state=%S"
               tag
               (selected-window)
               (buffer-name)
               (point)
               (and (boundp 'evil-state) evil-state))))
  
  (evil-define-motion flash-emacs-remote-motion (count)
    "Jump to remote location, read text object, apply operator.
Use as: yr<search><label>iw - yank inner word at target.

This bypasses Evil's operator state machine entirely:
1. Captures the current operator
2. Jumps to target with flash
3. Reads text object/motion from user
4. Calls the operator function directly with the range
5. Restores original position"
    :type exclusive
    :jump t
    (interactive "<c>")
    (let ((op evil-this-operator)
          (register evil-this-register)
          (start-win (selected-window))
          (start-buf (current-buffer))
          (start-pos (point))
          target-pos target-win target-buf)
      (flash-emacs-remote--log-position "motion-start")
      ;; Save state for restoration
      (when flash-emacs-remote-restore
        (flash-emacs-remote--save-state))
      ;; Abort this operator - we handle everything ourselves
      (setq evil-inhibit-operator t)
      (flash-emacs-remote--log-position "before-flash-jump")
      ;; Flash jump to target
      (let ((flash-emacs--remote-operation t))
        (flash-emacs-jump))
      (flash-emacs-remote--log-position "after-flash-jump")
      ;; Capture target
      (setq target-pos (point)
            target-win (selected-window)
            target-buf (current-buffer))
      (when flash-emacs-remote--debug
        (message "flash-remote: target captured: win=%S buf=%S pos=%d op=%S"
                 target-win (buffer-name target-buf) target-pos op))
      ;; Check if we actually moved to a different window
      (when (and flash-emacs-remote--debug (eq target-win start-win))
        (message "flash-remote: WARNING - still in same window!"))
      ;; Now read text object/motion and apply operator
      (let ((enters-insert nil))
        (when op
          (flash-emacs-remote--log-position "before-read-textobj")
          (let ((range (flash-emacs-remote--read-text-object-or-motion)))
            (flash-emacs-remote--log-position "after-read-textobj")
            (when range
              (when flash-emacs-remote--debug
                (message "flash-remote: got range=%S" range))
              (flash-emacs-remote--log-position "before-apply-op")
              (setq enters-insert
                    (flash-emacs-remote--apply-operator op range register))
              (flash-emacs-remote--log-position "after-apply-op"))))
        ;; Restore position unless operator entered insert mode
        ;; (change operator sets up its own hook for restoration)
        (when (and flash-emacs-remote-restore
                   (not enters-insert))
          (flash-emacs-remote--log-position "before-restore")
          (flash-emacs-remote--restore-state)
          (flash-emacs-remote--log-position "after-restore")))))

  ;; Bind in operator-pending mode
  (define-key evil-operator-state-map
              (kbd flash-emacs-remote-key)
              #'flash-emacs-remote-motion))

(provide 'flash-emacs-remote)

;;; flash-emacs-remote.el ends here
