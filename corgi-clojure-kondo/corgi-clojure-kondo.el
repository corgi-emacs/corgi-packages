;;; corgi-clojure-kondo.el --- clj-kondo configuration for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-clojure-kondo.el
;; Package-Requires: ((flymake-kondor))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comentary
;;
;; To use this module, you need to install clj-kondo from
;;   https://github.com/clj-kondo/clj-kondo and
;; and enable this module in your init.el file e.g.
;;
;; (let ((straight-current-profile 'corgi))
;;   ...
;;
;;   ;; Extensive setup for a good Clojure experience, including clojure-mode,
;;   ;; CIDER, and a modeline indicator that shows which REPLs your evaluations go
;;   ;; to.
;;   ;; Also contains `corgi/cider-pprint-eval-register', bound to `,,', see
;;   ;; `set-register' calls below.
;;   (use-package corgi-clojure)
;;   (use-package corgi-clojure-kondo)
;;
;;   ...
;;   }
;;
;;; Code:

(use-package flymake-kondor
  :config
  (add-hook 'clojure-mode-hook (lambda ()
	                         (flymake-kondor-setup)
                                 (flymake-mode +1))))

(provide 'corgi-clojure-kondo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-clojure-kondo.el ends here
