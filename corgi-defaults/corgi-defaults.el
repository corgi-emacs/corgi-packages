;;; corgi-defaults.el --- Sensible defaults for Emacs -*- lexical-binding: t -*-
;;
;; Filename: corgi-defaults.el
;; Description: Sensible defaults for Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Various things that really should have been configured this way out of the
;; box. This is mostly copied from Magnar Sveen's config, but stripped down.
;;
;;; Code:


;; Turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Smooth pixel-level scrolling
(when (display-graphic-p)
  (pixel-scroll-precision-mode 1))

(setq inhibit-startup-screen t)

;; Auto refresh buffers
;; Also auto refresh dired, but be quiet about it
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; Show keystrokes in progress in the minibuffer
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Don't use shift to mark things. This is Emacs, not Notepad.
(setq shift-select-mode nil)

;; Answering just 'y' or 'n' will do, instead of 'yes'/'no'. Sharp tools for
;; sharp minds.
(setq use-short-answers t)

;; UTF-8 please
(prefer-coding-system 'utf-8)

;; Always display line and column numbers
(column-number-mode 1)

;; Show line numbers in the fringe
(global-display-line-numbers-mode 1)

;; Lines should be 80 characters wide, not 72
(setq-default fill-column 80)

;; Never insert tabs
(setq-default indent-tabs-mode nil)

;; Show me empty lines after buffer end as a small bitmap in the fringe
(setq-default indicate-empty-lines t)

;; Keep long lines as a single line in the UI, truncating them, instead of
;; automatically (soft-)wrapping them. Use `toggle-truncate-lines' or
;; `visual-line-mode' to override this in the current buffer.
(setq-default truncate-lines t)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Remember where you were in a file. When re-visiting, the cursor is where you
;; last left it.
(save-place-mode 1)

;; Easily navigate CamelCased and snake_cased words, changes word motion command
;; boundaries
(global-subword-mode 1)

;; Only trigger garbage collection once 2MB has been allocated. Don't be too
;; stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 2000000)

;; Treat a simple period+space as the end of a sentence, rather than requiring
;; two spaces. This affects fill commands and functions like `sentence-end'.
(setq-default sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(setq uniquify-buffer-name-style 'forward)

;; Default is 4 / 12 , show a generous amount in the echo area, but do have a safety limit
(setq eval-expression-print-level 6
      eval-expression-print-length 300)

;; Ask when trying to save a file without a final newline, if it should be
;; added.
(setq-default require-final-newline 'ask)

;; Don't add a string to the killring if the previous one is identical.
(setq-default kill-do-not-save-duplicates t)

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

;; When Emacs is run in GUI mode, configure common Emoji fonts, making it more
;; likely that Emoji will work out of the box. These are appended to the stack,
;; so they are fallbacks. Existing font config takes precendence.
(when (display-graphic-p)
  (set-fontset-font t 'emoji "Apple Color Emoji" nil 'append)
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'emoji "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'emoji "Twemoji Mozilla" nil 'append)
  (set-fontset-font t 'emoji "JoyPixels" nil 'append)
  (set-fontset-font t 'emoji "Symbola" nil 'append))

;; Emacs will sometimes to try "ring the bell" to get your attention. We prefer
;; if it flashes the modeline. Customize `flash-face-attributes' if the default
;; red/white is too harsh. `flash-face-bell-function' is quite new (Emacs 31.1),
;; on older versions, do nothing.
(if (fboundp #'flash-face-bell-function)
    (setq ring-bell-function #'flash-face-bell-function)
  (setq ring-bell-function #'inhibit))

;; Configure mac modifiers to be what you expect -> Cmd = Control (e.g. type Cmd-q for C-q)
(when (eq system-type 'darwin)
  (setopt ns-command-modifier 'control))

(provide 'corgi-defaults)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-defaults.el ends here
