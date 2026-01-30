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
(defvar evil-state)
(defvar evil-inhibit-operator)
(defvar evil-operator-state-map)
(defvar evil-motion-state-map)
(defvar evil-normal-state-map)
(declare-function evil-yank "evil-commands")
(declare-function evil-define-motion "evil-macros")

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
    (when (and (window-live-p win) (buffer-live-p buf))
      (select-window win)
      (set-window-start win ws)
      (switch-to-buffer buf)
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
  
  ;; The trick: we define a motion that:
  ;; 1. Saves state
  ;; 2. Does flash jump (moves cursor)
  ;; 3. Captures operator
  ;; 4. Aborts current operator
  ;; 5. Re-invokes operator at new location
  ;;
  ;; This way, after `ys`, we're at the target and Evil is waiting
  ;; for a motion for `y` again.
  
  (evil-define-motion flash-emacs-remote-motion (count)
    "Jump to remote location and re-enter operator-pending mode.
Use as: ys<search><label>iw to yank inner word at target."
    :type inclusive
    :jump t
    (interactive "<c>")
    (let ((op evil-this-operator)
          (start-pos (point))
          (start-win (selected-window))
          (start-buf (current-buffer))
          target-pos target-win target-buf)
      ;; Save state for restoration BEFORE jumping
      (when flash-emacs-remote-restore
        (flash-emacs-remote--save-state))
      ;; Abort current operator
      (setq evil-inhibit-operator t)
      ;; Do the flash jump - this moves cursor to target
      (flash-emacs-jump)
      ;; Capture target position
      (setq target-pos (point)
            target-win (selected-window)
            target-buf (current-buffer))
      ;; Move back to start so Evil sees zero-width range (no-op)
      (select-window start-win)
      (switch-to-buffer start-buf)
      (goto-char start-pos)
      ;; Schedule the actual remote operation
      (when op
        (let ((op-key (car (where-is-internal op evil-normal-state-map))))
          (when op-key
            (run-at-time 0 nil
              (lambda ()
                ;; Go to target
                (select-window target-win)
                (switch-to-buffer target-buf)
                (goto-char target-pos)
                ;; Set up restoration hook
                (when flash-emacs-remote-restore
                  (flash-emacs-remote--start-waiting))
                ;; Simulate pressing the operator key
                (setq unread-command-events
                      (listify-key-sequence op-key)))))))))

  ;; Bind in operator-pending mode
  (define-key evil-operator-state-map
              (kbd flash-emacs-remote-key)
              #'flash-emacs-remote-motion))

(provide 'flash-emacs-remote)

;;; flash-emacs-remote.el ends here
