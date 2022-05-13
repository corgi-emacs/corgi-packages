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
;;   (use-package corgi-clojure)
;;   (use-package corgi-clojure-kondo)
;;
;;   ...
;;   }
;;
;;; Code:

(require 'flymake)

(straight-use-package 'flymake-kondor)

(add-hook 'clojure-mode-hook (lambda ()
	                       (flymake-kondor-setup)
                               (flymake-mode +1)))

(provide 'corgi-clojure-kondo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-clojure-kondo.el ends here
