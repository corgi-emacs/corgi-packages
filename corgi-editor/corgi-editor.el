;;; corgi-editor.el --- User interface configuration for Corgi
;;
;; Filename: corgi-editor.el
;; Package-Requires: ((use-package) (aggressive-indent) (avy) (company) (consult) (consult-selectrum) (diminish) (dumb-jump) (evil) (evil-cleverparens) (evil-collection) (evil-surround) (expand-region) (goto-last-change) (projectile) (rainbow-delimiters) (selectrum-prescient) (smartparens) (string-edit) (undo-fu) (which-key) (winum) (xclip))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package diminish
  :diminish
  elisp-slime-nav-mode
  eldoc-mode
  subword-mode)

(use-package selectrum-prescient
  :config
  (selectrum-mode +1)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package consult
  :config
  (setq consult-project-root-function #'projectile-project-root
        consult-async-default-split nil
        consult-find-command
        '("sh" "-c" "find . -not '(' -wholename '*/.*' -prune ')' | grep --perl-regexp -e ${1}" "--")))

(use-package consult-selectrum)

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
  (evil-collection-init)
  ;; Stop changing how last-sexp works. Even though we have evil-move-beyond-eol
  ;; set, this is still being added, and I can't figure out why. Resorting to
  ;; this hack.
  (cl-loop
   for fun
   in '(elisp--preceding-sexp cider-last-sexp pp-last-sexp)
   do (advice-mapc (lambda (advice _props) (advice-remove fun advice)) fun)))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

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
  (projectile-global-mode)
  (setq projectile-create-missing-test-files t))

(use-package dumb-jump)

(use-package goto-last-change)

(use-package expand-region)

(use-package string-edit)

(use-package xclip
  :config
  (unless (executable-find xclip-program)
    (xclip-mode t)))

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun magnars/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'magnars/create-non-existent-directory)

(defun corgi-editor/-on-buffer-change (win)
  (unless (and (minibufferp) (not evil-want-minibuffer))
    (evil-normal-state)))

(add-to-list 'window-buffer-change-functions #'corgi-editor/-on-buffer-change)

(provide 'corgi-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-editor.el ends here
