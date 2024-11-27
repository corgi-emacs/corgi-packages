;;; corgi-completion-ui.el --- Interactive completion user interface configuration for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-completion-ui.el
;; Package-Requires: ((use-package) (company) (counsel) (ivy) (ivy-prescient) (projectile) (swiper))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'use-package)

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
  :diminish
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

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-create-missing-test-files t))

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode))

;; silence byte compiler
(require 'ivy)

(provide 'corgi-completion-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-completion-ui.el ends here
