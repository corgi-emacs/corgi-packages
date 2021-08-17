;;; corgi-defaults.el --- Sensible defaults for Emacs
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
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8)   ; pretty
(set-terminal-coding-system 'utf-8)  ; pretty
(set-keyboard-coding-system 'utf-8)  ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8)        ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)
(set-default 'fill-column 80)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
;; (setq-default truncate-lines t)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 2000000)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; No electric indent
(setq electric-indent-mode nil)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Don't make backup~ files
;; (setq make-backup-files nil)

;; Put backups and auto-save files in subdirectories, so the
;; user-emacs-directory doesn't clutter
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; Stop asking about following symlinks to version controlled files
(setq vc-follow-symlinks t)

;; Configure common Emoji fonts, making it more likely that Emoji will work out of the box
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

(provide 'corgi-defaults)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-defaults.el ends here
