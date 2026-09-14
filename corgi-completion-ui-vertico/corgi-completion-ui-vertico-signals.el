;;; -*- no-byte-compile: t -*-

;; Signal overrides for `corgi-completion-ui-vertico', which uses consult,
;; vertico, corfu, and project.el to offer a variery of both minibuffer and
;; in-buffer completions.
;;
;; Gets added to the default signal files automatically if `corkey' is present.
;; If you are loading an explicit stack of signal files, add this after the main
;; `corgi-signals' but before `user-signals', so it overrides the former but not
;; the latter.
;;
;;   (corkey/load-and-watch nil '(corgi-signals corgi-completion-ui-vertico-signals user-signals))
;;
;; `consult-ripgrep' needs the `rg' executable; use `consult-grep' instead if
;; ripgrep is not installed.

((default ( :command/execute execute-extended-command
            :file/open find-file
            :file/open-recent consult-recent-file

            :buffer/switch consult-buffer
            :buffer/incremental-search consult-line

            :project/open-file project-find-file
            :project/switch project-switch-project
            :project/kill project-kill-buffers
            :project/incremental-search consult-ripgrep
            :project/switch-buffer consult-project-buffer

            :jump/identifier consult-imenu

            :toggle/completion corfu-mode)))
