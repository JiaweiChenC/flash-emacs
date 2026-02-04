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
(declare-function evil-normal-state "evil-states")

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
  
  ;; Map operator functions to their keys (more reliable than where-is-internal)
  (defvar flash-emacs-remote--operator-keys
    '((evil-delete . "d")
      (evil-yank . "y")
      (evil-change . "c")
      (evil-delete-char . "x")
      (evil-indent . "=")
      (evil-shift-left . "<")
      (evil-shift-right . ">")
      ;; Multi-key operators
      (evil-upcase . "gU")
      (evil-downcase . "gu")
      (evil-invert-char . "g~")
      (evil-invert-case . "g~")
      (evil-rot13 . "g?")
      (evil-fill . "gw")
      (evil-fill-and-move . "gq")
      (evil-join-whitespace . "gJ")
      ;; evil-org variants (same keys as standard operators)
      (evil-org-delete . "d")
      (evil-org-yank . "y")
      (evil-org-change . "c")
      (evil-org-< . "<")
      (evil-org-> . ">"))
    "Mapping of operator functions to their key sequences.")
  
  (defun flash-emacs-remote--get-operator-key (op)
    "Get the key sequence for operator OP."
    (or (cdr (assq op flash-emacs-remote--operator-keys))
        ;; Fallback to where-is-internal
        (let ((key (car (where-is-internal op evil-normal-state-map))))
          (when key (key-description key)))))
  
  (defun flash-emacs-remote--execute-at-target (op op-key target-pos target-win target-buf)
    "Execute operator OP with OP-KEY at TARGET-POS in TARGET-WIN/TARGET-BUF."
    (when (buffer-live-p target-buf)
      ;; Only switch window/buffer if target is in a different buffer
      (let ((same-buffer-p (eq target-buf (current-buffer))))
        (unless same-buffer-p
          (when (window-live-p target-win)
            (select-window target-win))
          (unless (eq (current-buffer) target-buf)
            (set-window-buffer (selected-window) target-buf))))
      (goto-char target-pos)
      ;; Ensure we're in normal state first
      (when (and (boundp 'evil-state) (not (eq evil-state 'normal)))
        (evil-normal-state))
      ;; Set up restoration hook
      (when flash-emacs-remote-restore
        (flash-emacs-remote--start-waiting))
      ;; For multi-key operators (like gU), temporarily bind to a single key
      ;; then use that key via unread-command-events
      (if (> (length op-key) 1)
          ;; Multi-key operator: temporarily bind to F13 (unlikely to be used)
          (let ((temp-key [f13]))
            (define-key evil-normal-state-map temp-key op)
            (setq unread-command-events (listify-key-sequence temp-key))
            ;; Schedule cleanup of the temporary binding
            (run-at-time 0.5 nil
                         (lambda ()
                           (define-key evil-normal-state-map temp-key nil))))
        ;; Single-key operator: use unread-command-events directly
        (setq unread-command-events (listify-key-sequence (kbd op-key))))))
  
  (evil-define-motion flash-emacs-remote-motion (count)
    "Jump to remote location and re-enter operator-pending mode.
Use as: dr<search><label>iw to delete inner word at target."
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
      (when (window-live-p start-win)
        (select-window start-win))
      (when (buffer-live-p start-buf)
        (set-buffer start-buf)
        (goto-char start-pos))
      ;; Schedule the actual remote operation
      (when op
        (let ((op-key (flash-emacs-remote--get-operator-key op)))
          (when op-key
            (run-at-time 0 nil
                         #'flash-emacs-remote--execute-at-target
                         op op-key target-pos target-win target-buf))))))

  ;; Bind in operator-pending mode
  (define-key evil-operator-state-map
              (kbd flash-emacs-remote-key)
              #'flash-emacs-remote-motion))

(provide 'flash-emacs-remote)

;;; flash-emacs-remote.el ends here
