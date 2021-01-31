;;; -*- no-byte-compile: t -*-
((corkey-local-mode ( :sexp/slurp-forward sp-forward-slurp-sexp
                      :sexp/barf-forward sp-forward-barf-sexp
                      :sexp/forward evil-cp-forward-sexp
                      :sexp/backward evil-cp-backward-sexp

                      :command/execute execute-extended-command

                      :file/open find-file
                      :file/save save-buffer
                      :file/save-all evil-write-all
                      :file/open-recent consult-recent-file

                      :buffer/switch consult-buffer
                      :buffer/incremental-search consult-line

                      :project/open-file projectile-find-file
                      :project/switch projectile-switch-project
                      :project/incremental-search consult-ripgrep

                      :jump/identifier consult-imenu
                      :jump/character avy-goto-char
                      :jump/last-change goto-last-change

                      :help/describe-key describe-key
                      :help/describe-variable describe-variable
                      :help/describe-function describe-function
                      :help/describe-mode describe-mode
                      :help/describe-bindings describe-bindings))

 (prog-mode ( :format/tab-indent indent-for-tab-command))

 (emacs-lisp-mode ( :eval/last-sexp eval-last-sexp
                    :eval/buffer eval-buffer
                    :eval/last-sexp-pprint pprint-to-buffer-last-sexp
                    :eval/region cider-eval-region
                    :repl/toggle ielm
                    :eval/last-sexp-pprint-comment pprint-to-buffer-last-sexp-to-current-buffer
                    :jump/definition xref-find-definitions
                    :jump/back xref-pop-marker-stack

                    :refactor/thread-first corgi/elisp-thread-first-all
                    :refactor/thread-last corgi/elisp-thread-last-all))

 (inferior-emacs-lisp-mode ( :repl/toggle corgi/switch-to-last-elisp-buffer))

 (clojure-mode ( :sexp/slurp-forward sp-forward-slurp-sexp
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
               :eval/registry-pprint corgi/cider-pprint-register
               :eval/interrupt cider-interrupt
               :eval/up-to-point cider-eval-sexp-up-to-point

               :repl/toggle cider-switch-to-repl-buffer
               :repl/quit cider-quit
               :repl/quit-all corgi/cider-quit-all
               :repl/other cider-repl-switch-to-other
               :repl/connect cider-connect
               :repl/jack-in ("Jack-in Clojure" cider-jack-in-clj)
               :repl/jack-in-other ("Jack in ClojureScript" cider-jack-in-clj)
               :repl/jack-in-all ("Jack in Clj+Cljs" cider-jack-in-clj&cljs)
               ;; TODO: this should clear the BUFFER, not just the last output
               :repl/clear cider-find-and-clear-repl-output

               :jump/definition cider-find-var
               :jump/back cider-pop-back
               :jump/ns cider-find-ns
               ))

 (inf-clojure-minor-mode ( :eval/last-sexp inf-clojure-eval-last-sexp
                           :repl/toggle inf-clojure))

 (cider-repl-mode ( :repl/toggle cider-switch-to-last-clojure-buffer
                    :repl/quit cider-quit
                    :repl/other cider-repl-switch-to-other
                    :repl/quit-all corgi/cider-quit-all
                    :repl/connect cider-connect
                    :repl/jack-in ("Jack-in Clojure" cider-jack-in-clj)
                    :repl/jack-in-other ("Jack in ClojureScript" cider-jack-in-clj)
                    :repl/jack-in-all ("Jack in Clj+Cljs" cider-jack-in-clj&cljs)
                    :repl/clear cider-repl-clear-buffer

                    :eval/registry-pprint corgi/cider-pprint-register

                    :jump/definition cider-find-var
                    :jump/back cider-pop-back
                    :jump/ns cider-find-ns))

 (c-mode ( :jump/definition xref-find-definitions
           :jump/back xref-pop-marker-stack))

 (sql-mode ( :eval/last-sexp sql-send-paragraph
             :eval/buffer sql-send-buffer
             :eval/region sql-send-region)))
