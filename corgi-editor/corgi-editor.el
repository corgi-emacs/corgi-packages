;;; corgi-editor.el --- Editing user interface configuration for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-editor.el
;; Package-Requires: ((use-package) (aggressive-indent) (avy) (diminish) (dumb-jump) (expand-region) (rainbow-delimiters) (smartparens) (string-edit-at-point) (which-key) (winum) (xclip))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'use-package)

(use-package diminish
  :diminish
  elisp-slime-nav-mode
  eldoc-mode
  subword-mode)

(use-package avy
  :defer t)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (setq which-key-sort-order 'which-key-prefix-then-key-order))

(use-package winum
  :config (winum-mode 1))

(use-package smartparens
  :init (require 'smartparens-config)
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((clojure-mode
          emacs-lisp-mode
          lisp-data-mode
          js-mode
          piglet-mode)
         . aggressive-indent-mode))

(use-package rainbow-delimiters
  :hook ((cider-repl-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode
          inferior-emacs-lisp-mode)
         . rainbow-delimiters-mode))

(use-package dumb-jump)

(use-package expand-region)

(use-package string-edit-at-point)

;; silence byte compiler
(require 'winum)
(require 'smartparens)

(use-package xclip
  :if (not (display-graphic-p))
  :config (with-no-warnings (xclip-mode t)))

;; Offer to create parent directories if they do not exist
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
