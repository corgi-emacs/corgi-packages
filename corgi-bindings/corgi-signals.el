;;; -*- no-byte-compile: t -*-

;; This is Corgi's built-in "signals" file, it defines how abstract signals like
;; `:eval/buffer' map to concrete commands like `cider-eval-buffer', based on
;; the major (or sometimes minor) mode. This is how Corgi achieves consistency
;; in key bindings across languages.

((default ( :command/execute counsel-M-x

            :file/open counsel-find-file
            :file/save save-buffer
            :file/save-all evil-write-all
            :file/open-recent counsel-recentf

            :buffer/switch switch-to-buffer
            :buffer/incremental-search swiper

            :project/open-file projectile-find-file
            :project/switch projectile-switch-project
            :project/kill projectile-kill-buffers
            :project/incremental-search counsel-git-grep
            :project/switch-buffer projectile-switch-to-buffer

            :jump/identifier counsel-imenu
            :jump/character avy-goto-char
            :jump/last-change goto-last-change

            :help/describe-key describe-key
            :help/describe-variable describe-variable
            :help/describe-function describe-function
            :help/describe-mode describe-mode
            :help/describe-bindings describe-bindings

            :sexp/slurp-forward sp-forward-slurp-sexp
            :sexp/barf-forward sp-forward-barf-sexp
            :sexp/forward evil-cp-forward-sexp
            :sexp/backward evil-cp-backward-sexp

            :toggle/read-only read-only-mode
            :toggle/soft-word-wrap visual-line-mode
            :toggle/hard-word-wrap auto-fill-mode
            :toggle/line-numbers display-line-numbers-mode
            :toggle/aggressive-indent aggressive-indent-mode
            :toggle/debug-on-quit toggle-debug-on-quit
            :toggle/debug-on-error toggle-debug-on-error
            :toggle/completion company-mode))

 (prog-mode ( :format/tab-indent indent-for-tab-command
              :jump/definition xref-find-definitions
              :jump/back xref-pop-marker-stack))

 (emacs-lisp-mode ( :eval/last-sexp eval-last-sexp
                    :eval/buffer eval-buffer
                    :eval/last-sexp-pprint pprint-to-buffer-last-sexp
                    :eval/region eval-region
                    :eval/outer-sexp eval-defun
                    :repl/toggle ielm
                    :eval/last-sexp-pprint-comment pprint-to-buffer-last-sexp-to-current-buffer

                    :refactor/thread-first corgi/elisp-thread-first-all
                    :refactor/thread-last corgi/elisp-thread-last-all))

 (inferior-emacs-lisp-mode ( :repl/toggle corgi/switch-to-last-elisp-buffer))

 (lisp-mode ( :eval/last-sexp lisp-eval-last-sexp
              :eval/buffer lisp-eval-buffer
              :eval/outer-sexp lisp-eval-defun
              :eval/region lisp-eval-region
              :repl/toggle run-lisp
              :repl/connect run-lisp))

 (scheme-mode ( :eval/last-sexp geiser-eval-last-sexp
                :eval/buffer geiser-eval-buffer
                :eval/region geiser-eval-region))

 (clojure-mode ( :repl/connect ("Connect clj" cider-connect)
                 :repl/connect-other ("Connect cljs" cider-connect-cljs)
                 :repl/connect-all ("Connect clj&cljs" cider-connect-clj&cljs)
                 :repl/jack-in ("Jack-in Clojure" cider-jack-in-clj)
                 :repl/jack-in-other ("Jack in ClojureScript" cider-jack-in-cljs)
                 :repl/jack-in-all ("Jack in Clj+Cljs" cider-jack-in-clj&cljs)
                 :sexp/slurp-forward sp-forward-slurp-sexp
                 :sexp/barf-forward sp-forward-barf-sexp
                 :sexp/forward clojure-forward-logical-sexp
                 :sexp/backward clojure-backward-logical-sexp

                 :refactor/thread-first clojure-thread-first-all
                 :refactor/thread-last clojure-thread-last-all
                 :refactor/unwind-thread clojure-unwind-all

                 :refactor/sort-namespace-declaration clojure-sort-ns
                 :refactor/add-missing cljr-add-missing-libspec
                 :refactor/extract-function cljr-extract-function))

 (cider-mode ( :eval/last-sexp cider-eval-last-sexp
               :eval/last-sexp-pprint cider-pprint-eval-last-sexp
               :eval/last-sexp-pprint-comment cider-pprint-eval-last-sexp-to-comment
               :eval/ns-form cider-eval-ns-form
               :eval/last-sexp-replace cider-eval-last-sexp-and-replace
               :eval/buffer cider-eval-buffer
               :eval/region cider-eval-region
               :eval/registry-pprint corgi/cider-pprint-eval-register
               :eval/interrupt cider-interrupt
               :eval/outer-sexp cider-eval-defun-at-point
               :eval/up-to-point cider-eval-sexp-up-to-point

               :repl/toggle cider-switch-to-repl-buffer
               :repl/quit cider-quit
               :repl/quit-all corgi/cider-quit-all
               :repl/other cider-repl-switch-to-other
               ;; TODO: this should clear the BUFFER, not just the last output
               :repl/clear cider-find-and-clear-repl-output
               :repl/set-ns cider-repl-set-ns
               :repl/toggle-message-logging nrepl-toggle-message-logging

               :jump/ns cider-find-ns

               :link-repl/project sesman-link-with-project
               :link-repl/buffer sesman-link-with-buffer
               :link-repl/directory sesman-link-with-directory
               :link-repl/least-specific sesman-link-with-least-specific
               :link-repl/unlink sesman-unlink

               :trace/toggle-var cider-toggle-trace-var
               :trace/toggle-ns cider-toggle-trace-ns))

 (inf-clojure-minor-mode ( :eval/last-sexp inf-clojure-eval-last-sexp
                           :eval/buffer inf-clojure-eval-buffer
                           :eval/region inf-clojure-eval-region
                           :eval/outer-sexp inf-clojure-eval-defun

                           :repl/toggle inf-clojure-switch-to-repl
                           :repl/quit inf-clojure-quit))

 (cider-repl-mode ( :repl/toggle cider-switch-to-last-clojure-buffer
                    :repl/quit cider-quit
                    :repl/other cider-repl-switch-to-other
                    :repl/quit-all corgi/cider-quit-all
                    :repl/connect ("Connect clj" cider-connect)
                    :repl/connect-other ("Connect cljs" cider-connect-cljs)
                    :repl/connect-all ("Connect clj&cljs" cider-connect-clj&cljs)
                    :repl/jack-in ("Jack-in Clojure" cider-jack-in-clj)
                    :repl/jack-in-other ("Jack in ClojureScript" cider-jack-in-clj)
                    :repl/jack-in-all ("Jack in Clj+Cljs" cider-jack-in-clj&cljs)
                    :repl/clear cider-repl-clear-buffer
                    :repl/toggle-message-logging nrepl-toggle-message-logging
                    :eval/registry-pprint corgi/cider-pprint-eval-register
                    :jump/ns cider-find-ns))

 (piglet-mode ( :eval/last-sexp pdp-eval-last-sexp
                :eval/outer-sexp pdp-eval-outer-sexp
                :eval/region pdp-eval-region
                :eval/buffer pdp-eval-buffer
                :eval/last-sexp-pprint pdp-eval-last-sexp-to-result-buffer
                :jump/definition pdp-jump-to-definition))

 (sql-mode ( :repl/toggle sql-show-sqli-buffer
             :eval/last-sexp sql-send-paragraph
             :eval/outer-sexp sql-send-paragraph
             :eval/buffer sql-send-buffer
             :eval/region sql-send-region))

 (dired-mode (:toggle/read-only wdired-change-to-wdired-mode))
 (wdired-mode (:toggle/read-only wdired-finish-edit)))
