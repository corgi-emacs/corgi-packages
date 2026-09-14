;;; corgi-editor.el --- Editing user interface configuration for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-editor.el
;; Package-Requires: ((use-package) (aggressive-indent) (avy) (diminish) (dumb-jump) (expand-region) (rainbow-delimiters) (smartparens) (string-edit-at-point) (which-key) (winum) (xclip))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'use-package)

;; Diminish lets us hide certain minor-modes from the mode line, to keep it
;; clean. We do this here for some built-ins we enable, for other packages it's
;; set up in the respective `use-package' declaration.
(use-package diminish
  :diminish
  eldoc-mode
  subword-mode)

;; Jump to a visible character or word with a two-key sequence (`avy-goto-char',
;; Corgi binding: `SPC j j').
(use-package avy
  :defer t)

;; After a prefix key, show the available continuations in a popup. Built in
;; from Emacs 30, otherwise we load the package.
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (setq which-key-sort-order 'which-key-prefix-then-key-order))

;; Number the visible windows and allow switching to one by its number. (Corgi binding: `SPC <num>')
(use-package winum
  :config (winum-mode 1))

;; Structural editing: automatically insert/delete matching delimiters and
;; provide slurp/barf commands. `smartparens-config' sets up the default pairs;
;; Enabled in any programming mode. (Corgi binding: `<' slurp, `>' burf)
(use-package smartparens
  :diminish smartparens-mode
  :config (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode))

;; Try to keep code correctly indented at all times. Enables Emacs's built-in
;; `electric-indent' behavior, so it indents when entering a newline, but also
;; re-indents the affected region after edit commands.
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook (prog-mode . aggressive-indent-mode))

;; Color parentheses according to nesting depth, in Lisp modes and the CIDER
;; REPL.
(use-package rainbow-delimiters
  :hook ((cider-repl-mode
          inferior-emacs-lisp-mode
          prog-mode)
         . rainbow-delimiters-mode))

;; Fallback "jump to definition" based on heuristics rather than a language
;; server; integrates with `xref'.
;; TODO: not bound yet in corgi-bindings
(use-package dumb-jump
  :defer t)

;; Expand the region by semantic units (word, sexp, defun, ...). (Corgi binding: `<M-up>' / `<M-down>' (in normal mode))
(use-package expand-region
  :defer t)

;; Edit a string literal in a dedicated buffer instead of in place. (Corgi
;; binding: `SPC o s' (open string) / `C-c C-c' to finish)
(use-package string-edit-at-point
  :defer t)

;; In a terminal, use an executable program to talk to the desktop clipboard.
(when (not (display-graphic-p))
  (use-package xclip
    :config
    (with-no-warnings (xclip-mode t))))

;; Offer to create parent directories if they do not exist, when visiting a file
;; whose parent directory is missing, so the user is offered to create it.
;;
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun corgi/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'corgi/create-non-existent-directory)

(provide 'corgi-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-editor.el ends here
