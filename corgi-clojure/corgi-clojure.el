;;; corgi-clojure.el --- Clojure configuration for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-clojure.el
;; Package-Requires: ((use-package) (cider) (clj-ns-name) (clojure-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :config
  (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, we
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil))

(use-package cider
  :diminish cider-mode
  :config
  (setq cider-preferred-build-tool 'clojure-cli)

  (defadvice cider-find-var (before add-evil-jump activate)
    (evil-set-jump)))

;; (use-package clj-refactor
;;   :after (cider)
;;   :diminish clj-refactor-mode
;;   :config
;;   (setq cljr-cljc-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
;;         cljr-cljs-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
;;         cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
;;         cljr-eagerly-build-asts-on-startup nil
;;         cljr-warn-on-eval nil)
;;   :hook ((clojurex-mode-hook
;;           clojurescript-mode-hook
;;           clojurec-mode-hook
;;           clojure-mode-hook)
;;          . clj-refactor-mode))

(use-package clj-ns-name
  :config
  (clj-ns-name-install))

(provide 'corgi-clojure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-clojure.el ends here
