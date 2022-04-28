;;; corgi-editor.el --- User interface configuration for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-editor.el
;; Package-Requires: ((use-package) (aggressive-indent) (avy) (company) (counsel) (diminish) (dumb-jump) (evil) (evil-cleverparens) (evil-collection) (evil-surround) (expand-region) (goto-last-change) (ivy) (projectile) (rainbow-delimiters) (smartparens) (smex) (string-edit) (swiper) (undo-fu) (which-key) (winum) (xclip))
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

(use-package ivy
  :defer 0.1
  :diminish
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line))

(use-package counsel
  :after (ivy)
  :config
  (counsel-mode)
  ;; This ensures that SPC f r (counsel-recentf, show recently opened files)
  ;; actually works
  (recentf-mode 1))

(use-package ivy-prescient
  :after (ivy)
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package swiper
  :after (ivy)
  :bind (("C-s" . swiper)))

(use-package avy)

(use-package undo-fu)

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (evil-set-undo-system 'undo-fu)
  (setq evil-move-cursor-back nil
        evil-move-beyond-eol t
        evil-want-fine-undo t
        evil-mode-line-format 'before
        evil-normal-state-cursor '(box "orange")
        evil-insert-state-cursor '(box "green")
        evil-visual-state-cursor '(box "#F86155")
        evil-emacs-state-cursor  '(box "purple"))

  ;; Prevent evil-motion-state from shadowing previous/next sexp
  (require 'evil-maps)
  (define-key evil-motion-state-map "L" nil)
  (define-key evil-motion-state-map "M" nil))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

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

;; We don't actually enable cleverparens, because most of their bindings we
;; don't want, we install our own bindings for specific sexp movements
(use-package evil-cleverparens
  :after (evil smartparens))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode)
         . aggressive-indent-mode))

(use-package rainbow-delimiters
  :hook ((cider-repl-mode
          clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode
          inferior-emacs-lisp-mode)
         . rainbow-delimiters-mode))

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-create-missing-test-files t))

(use-package dumb-jump)

(use-package goto-last-change)

(use-package expand-region)

(use-package string-edit)

;; silence byte compiler
(require 'evil)
(require 'evil-core)
(require 'winum)
(require 'evil-collection)
(require 'ivy)

(when (and (not (display-graphic-p))
           (executable-find "xclip"))
  (use-package xclip
    :config
    (when (executable-find xclip-program)
      (with-no-warnings
        (xclip-mode t)))))

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun magnars/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'magnars/create-non-existent-directory)

(defvar corgi-editor--last-buffer
  nil
  "The last current buffer.")

(defun corgi-editor/-on-buffer-change (&optional _win)
  (unless (or (and (minibufferp) (not evil-want-minibuffer))
              (eq (current-buffer) corgi-editor--last-buffer))
    (setq corgi-editor--last-buffer (current-buffer))
    (evil-normal-state)))

(if (boundp 'window-buffer-change-functions)
    ;; Emacs 27.1+ only
    (add-hook 'window-buffer-change-functions #'corgi-editor/-on-buffer-change)
  (add-hook 'post-command-hook #'corgi-editor/-on-buffer-change))

(provide 'corgi-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-editor.el ends here
