;;; corgi-defaults.el --- Sensible defaults for Emacs -*- lexical-binding: t -*-
;;
;; Filename: corgi-defaults.el
;; Description: Sensible defaults for Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Various things that really should have been configured this way
;; out of the box. This is mostly copied from Magnar Sveen's config,
;; but stripped down.
;;
;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

;; Auto refresh buffers
;; Also auto refresh dired, but be quiet about it
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Don't use shift to mark things. This is Emacs, not Notepad.
(setq shift-select-mode nil)

;; Answering just 'y' or 'n' will do. Sharp tools for sharp minds.
(setq use-short-answers t)

;; UTF-8 please
(prefer-coding-system 'utf-8)

;; Always display line and column numbers
(column-number-mode 1)

;; Lines should be 80 characters wide, not 72
(setq-default fill-column 80)

;; Never insert tabs
(setq-default indent-tabs-mode nil)

;; Show me empty lines after buffer end
(setq-default indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
;; (setq-default truncate-lines t)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 2000000)

;; Sentences do not need double spaces to end. Period.
(setq-default sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Default is 4 / 12 , show a generous amount in the echo area, but do have a safety limit
(setq eval-expression-print-level 6
      eval-expression-print-length 300)

;; Put backups and auto-save files in subdirectories, so the
;; user-emacs-directory doesn't clutter
(let ((backup-dir (expand-file-name "backups" user-emacs-directory))
      (auto-save-dir (expand-file-name "auto-save-list/" user-emacs-directory))
      (tramp-auto-save-dir (expand-file-name "auto-save-list/tramp/" user-emacs-directory)))
  (make-directory backup-dir t)
  (make-directory auto-save-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        tramp-auto-save-directory tramp-auto-save-dir))

;; Stop asking about following symlinks to version controlled files
(setq vc-follow-symlinks t)

;; When Emacs is ran in GUI mode, configure common Emoji fonts, making it more
;; likely that Emoji will work out of the box
(when (display-graphic-p)
  (set-fontset-font t 'emoji "Apple Color Emoji" nil 'append)
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'emoji "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'emoji "Twemoji Mozilla" nil 'append)
  (set-fontset-font t 'emoji "Joypixels" nil 'append)
  (set-fontset-font t 'emoji "Symbola" nil 'append))

(setq ring-bell-function #'flash-face-bell-function)

;; Configure mac modifiers to be what you expect -> Cmd = Control (e.g. type Cmd-q for C-q)
(when (eq system-type 'darwin)
  (setopt ns-command-modifier 'control))

(provide 'corgi-defaults)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-defaults.el ends here
