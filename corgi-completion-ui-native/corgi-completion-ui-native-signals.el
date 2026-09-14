;;; -*- no-byte-compile: t -*-

;; Signal overrides for `corgi-completion-ui-native'. Use basic Emacs built-ins,
;; or functions from `project.el' (also built-in). `fido' hooks into these to
;; provide minibuffer completion.
;;
;; Gets added to the default signal files automatically if `corkey' is present.
;; If you are loading an explicit stack of signal files, add this after the main
;; `corgi-signals' but before `user-signals', so it overrides the former but not
;; the latter.
;;
;;   (corkey/load-and-watch nil '(corgi-signals corgi-completion-ui-native-signals user-signals))

((default ( :command/execute execute-extended-command
            :file/open find-file
            :file/open-recent recentf-open

            :buffer/switch switch-to-buffer
            :buffer/incremental-search isearch-forward

            :project/open-file project-find-file
            :project/switch project-switch-project
            :project/kill project-kill-buffers
            :project/incremental-search project-search
            :project/switch-buffer project-switch-to-buffer

            :jump/identifier imenu

            :toggle/completion completion-preview-mode)))
