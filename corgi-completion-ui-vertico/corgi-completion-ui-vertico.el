;;; corgi-completion-ui-vertico.el --- Vertico/Corfu completion UI for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-completion-ui-vertico.el
;; Package-Requires: ((use-package) (vertico) (orderless) (marginalia) (consult) (embark) (corfu) (cape))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Alternative to `corgi-completion-ui', using Consult/Vertico/Corfu and
;; friends.
;;
;; This is an experimental package, the exact stack, and the name of the
;; package, will likely still change.
;;
;; - consult    search and navigation commands built on `completing-read'
;; - vertico    vertical minibuffer completion
;; - corfu      in-buffer completion, with cape for extra completion sources
;; - orderless  space-separated, order-independent matching
;; - marginalia annotations for completion candidates
;; - embark     contextual actions on candidates
;;
;;; Code:

(require 'use-package)

;; Minibuffer completion
(use-package vertico
  :config
  (vertico-mode 1))

;; Order-independent matching.
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion)))))

;; Annotate completion candidates with useful metadata (size, buffer state, etc.).
(use-package marginalia
  :config
  (marginalia-mode 1))

;; Search and navigation commands built on `completing-read'.
(use-package consult)

;; Contextual actions on the candidate at point.  `C-.' is unbound by default.
(use-package embark
  :functions (embark-act)
  :init
  ;; FIXME: do this through corkey signals
  (keymap-set minibuffer-local-map "C-." #'embark-act)
  (keymap-set global-map "C-." #'embark-act))

;; In-buffer completion.
(use-package corfu
  :defines (corfu-auto corfu-cycle)
  :functions (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-cycle t)
  (global-corfu-mode 1))

;; Extra completion sources. `cape-file' completes file names in code (for
;; example in strings), which the language modes usually don't provide.
(use-package cape
  :functions (cape-file)
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-file nil t))))

(with-eval-after-load 'corkey
  (corkey-register-signal-file 'corgi-completion-ui-vertico-signals))

(provide 'corgi-completion-ui-vertico)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-completion-ui-vertico.el ends here
