;;; -*- no-byte-compile: t -*-

;;; Usage:

;; This file contains Corgi's built-in key bindings. Your own bindings should go
;; in `user-keys.el' in your `user-emacs-directory'. Use `SPC f e k'
;; (`corgi/open-user-keys-file') to create a stub version and open it.
;;
;; Bindings here are nested, e.g. `("SPC" ("b" ("k" kill-buffer)))' means that
;; "space" followed by "b" and then "k" will invoke `M-x kill-buffer'.
;;
;; You can add a descriptions before the command, this will show up in a pop-up
;; when you press the prefix key and wait a bit. (This uses which-key)
;;
;; Instead of a prefix key you can use a symbol like `normal' or `insert', which
;; designates the Evil state (what vim calls the mode). `global' means any
;; state, `normal|visual' means either normal or visual.
;;
;; Instead of a command like `kill-buffer' you can put a keyword like
;; `:eval/buffer'. This is called a "signal". In the `corgi-signals' (or
;; `user-signals') file these are bound to specific commands based on the major
;; mode. E.g. in Emacs Lisp `:eval/buffer' means `eval-buffer', whereas in
;; Clojure it means `cider-eval-buffer'.
;;
;; If you want to alter Corgi's bindings directly then copy this file to your
;; `user-emacs-directory', and Corgi will pick up your local version instead of
;; the version it ships with.

;;; Grammar:

;; BINDINGS  := '(bindings' <def>+ ')'
;; <def>     := '(' <key> <doc> <target> ')' | '(' <prefix> <def> + ')'
;; <target>  := <signal> | <command>
;; <prefix>  := <state> | <key>
;; <state>   := 'normal' | 'insert' | 'visual' | 'emacs' | 'motion' | 'global'
;; <key>     := stringp
;; <doc>     := stringp
;; <signal>  := keywordp
;; <command> := symbolp

(bindings
 (global
  ;; This needs revisiting, since it breaks org-cycle. We translate TAB into
  ;; <tab>, but it seems the default (?) local-function-key-map translates <tab>
  ;; into TAB, so as long as we bind commands to `TAB' (and not `<tab>'), it
  ;; should already "just work" in GUI and terminal
  ;;
  ;; ("TAB" "Emulate <tab> on terminals" corgi/emulate-tab)
  ("M-[ 1 ~" "Home key" beginning-of-line)
  ("M-[ 4 ~" "End key" end-of-line)
  ("<select>" "Home key" end-of-line))

 (normal|visual
  ("TAB" "Indent" :format/tab-indent)
  (">" "Slurp" :sexp/slurp-forward)
  ("<" "Barf" :sexp/barf-forward)

  ;; These override evil-window-bottom / evil-window-top. I don't like that we
  ;; are redefining built-in default vim bindings, but we do need a single key
  ;; motion for forward/backward sexp. Note that to make this work we remove
  ;; these bindings in init.el from evil-motion-state-map
  ("L" "Forward sexp" :sexp/forward)
  ("H" "Backward sexp" :sexp/backward)
  ("M-l" "End of outer sexp" evil-cp-end-of-defun)
  ("M-h" "Start of outer sexp" evil-cp-beginning-of-defun)

  ;; Leaving these evil-cleverparens style bindings out for now, instead
  ;; sticking to the default vim-style bindings
  ;; ("[" "Previous opening delimiter" evil-cp-previous-opening)
  ;; ("]" "Next closing delimiter" evil-cp-next-closing)
  ;; ("{" "Next opening delimiter" evil-cp-next-opening)
  ;; ("}" "Previous closing delimiter" evil-cp-previous-closing)
  ;; ("(" "Backward up sexp" evil-cp-backward-up-sexp)
  ;; (")" "Up sexp" evil-cp-up-sexp)

  ("<M-up>" "Expand region" er/expand-region)
  ("<M-down>" "Contract region" er/contract-region)

  ("gc" "Comment region" comment-region)
  ("gC" "Uncomment region" uncomment-region)

  ("C-x" ("C-e" "Eval form before cursor" :eval/last-sexp))

  ("SPC" "Global leader key"
   ("b" "Buffer commands"
    ("b" "Switch buffer" ivy-switch-buffer)
    ("d" "Kill current buffer" kill-current-buffer)
    ("k" "Pick & kill" kill-buffer)
    ("l" "List buffers" list-buffers)
    ("r" "Rename buffer" rename-buffer))

   ("f" "File commands"
    ("f" "Find file" :file/open)
    ("s" "Save file" :file/save)
    ("S" "Save all" :file/save-all)
    ("r" "Recently opened files" :file/open-recent)
    ("A" "Find alternate file" find-alternate-file)
    ("e" "Emacs files"
     ("i" "Open init.el" corgi/open-init-el)
     ("k" "Open user-keys key bindings file" corgi/open-user-keys-file)
     ("s" "Open user-signals signals file" corgi/open-user-signals-file)
     ("K" "Open corgi-key key bindings file" corgi/open-keys-file)
     ("S" "Open corgi-signals signals file" corgi/open-signals-file)
     ("v" "Open Straight's default version file" corgi/open-straight-default-versions-file)
     ("V" "Open Straight's version file installed by Corgi" corgi/open-straight-corgi-versions-file)
     ("l" "Find library" find-library)))

   ("s" "Search commands"
    ("s" "Search in buffer" :buffer/incremental-search))

   ("p" "Project"
    ("f" "Find file" :project/open-file)
    ("p" "Switch project" :project/switch)
    ("k" "Kill buffers" :project/kill)
    ("s" "Search in project" :project/incremental-search)
    ("b" "Switch to project buffer" :project/switch-buffer))

   ("g" "Git"
    ("s" "Magit Status" magit-status))

   ("h" "Help"
    ("d" "Describe"
     ("k" "Describe key" :help/describe-key)
     ("v" "Describe variable" :help/describe-variable)
     ("f" "Describe function" :help/describe-function)
     ("m" "Describe mode" :help/describe-mode)
     ("b" "Describe bindings" :help/describe-bindings)))

   ;; TODO: Unify this with , g (go...)
   ("j" "Jump"
    ("i" "Jump in buffer" :jump/identifier)
    ("j" "Jump to character" :jump/character)
    ("c" "Jump to last change" :jump/last-change))

   ("w" "Windows"
    ("TAB" "Alternate window" alternate-window)
    ("0" "Delete window" delete-window)
    ("1" "Delete other windows" delete-other-windows)
    ("2" "Two column layout" corgi/double-columns)
    ("/" "Split window right" split-window-right)
    ("-" "Split window below" split-window-below)
    ("o" "Go to other window" other-window)
    ("d" "Delete window" delete-window))

   ("t" "Toggle modes"
    ("a" "Toggle aggressive indent mode" :toggle/aggressive-indent)
    ("c" "Toggle completion" :toggle/completion)
    ("e" "Toggle debug on error" :toggle/debug-on-error)
    ("l" "Toggle line numbers" :toggle/line-numbers)
    ("q" "Toggle debug on quit" :toggle/debug-on-quit)
    ("r" "Toggle read-only" :toggle/read-only)
    ("w" "Toggle soft word-wrap" :toggle/soft-word-wrap)
    ("W" "Toggle hard word-wrap" :toggle/hard-word-wrap)
    ("W" "Toggle wrap at fill-column" visual-fill-column-mode))

   ("x" "Text editing"
    ("t" "Transpose sexps" transpose-sexps)
    ("s" "Splice backwards" sp-splice-sexp-killing-backward))

   ("SPC" "Execute command (M-x)" :command/execute)
   ("u" "Universal prefix" universal-argument)
   ("TAB" "Switch to previous buffer" corgi/switch-to-previous-buffer)
   ("1" "Go to window 1" winum-select-window-1)
   ("2" "Go to window 2" winum-select-window-2)
   ("3" "Go to window 3" winum-select-window-3)
   ("4" "Go to window 4" winum-select-window-4)
   ("5" "Go to window 5" winum-select-window-5)
   ("6" "Go to window 6" winum-select-window-6)
   ("7" "Go to window 7" winum-select-window-7)
   ("8" "Go to window 8" winum-select-window-8)
   ("9" "Go to window 9" winum-select-window-9)
   ("0" "Go to window 0 or 10" winum-select-window-0-or-10)
   )

  ("," "Project specific leader key"

   ("j" "Jack-in"
    ("j" "Jack in" :repl/jack-in)
    ("o" "Jack in other" :repl/jack-in-other)
    ("a" "Jack in all" :repl/jack-in-all))

   ("c" "Connect to REPL"
    ("c" "Connect to REPL" :repl/connect)
    ("o" "Connect to Other REPL Type" :repl/connect-other)
    ("a" "Connect to All REPL Types" :repl/connect-all))

   ("s" "REPL"
    ("s" "Toggle REPL" :repl/toggle)
    ("q" "Quit current REPL" :repl/quit)
    ("Q" "Quit all active REPLs" :repl/quit-all)
    ("o" "Switch to Other REPL" :repl/other)
    ("l" "Clear REPL" :repl/clear)
    ("n" "Set namespace" :repl/set-ns)
    ("m" "Toggle message logging" :repl/toggle-message-logging))

   ("l" "Link to REPL"
    ("p" "Link with project" :link-repl/project)
    ("b" "Link with buffer" :link-repl/buffer)
    ("d" "Link with directory" :link-repl/directory)
    ("l" "Link least specific" :link-repl/least-specific)
    ("u" "Unlink" :link-repl/unlink))

   ("e" "Evaluate expressions"
    ("b" "Eval buffer" :eval/buffer)
    ("e" "Eval form before cursor" :eval/last-sexp)
    ("p" "Eval and pretty print" :eval/last-sexp-pprint)
    ;; TODO: make this consistent, in clojure buffers it prints to a comment, in elisp it inserts directly
    ("P" "Eval to comment" :eval/last-sexp-pprint-comment)
    ("n" "Eval ns form" :eval/ns-form)
    ("r" "Eval region" :eval/region)
    ("i" "Interrupt eval" :eval/interrupt)
    ("-" "Eval up to point" :eval/up-to-point))

   ("g" "Go places"
    ("g" "Go to definition" :jump/definition)
    ("b" "Go back" :jump/back)
    ("n" "Go to namespace" :jump/ns)
    ("t" "Go to test/implemenentation" projectile-toggle-between-implementation-and-test))

   ("r" "Refactor"
    ("t" "Threading"
     ("f" "Thread first" :refactor/thread-first)
     ("l" "Thread last"  :refactor/thread-last)
     ("u" "Unwind thread" :refactor/unwind-thread))
    ("s" "Sort ..."
     ("n" "Sort namespace declaration" :refactor/sort-namespace-declaration))
    ("a" "Add ..."
     ("m" "Add missing" :refactor/add-missing))
    ("e" "Extract..."
     ("f" "Extract function" :refactor/extract-function)))

   ("t" "Trace"
    ("v" "Toggle trace var" :trace/toggle-var)
    ("n" "Toggle trace ns" :trace/toggle-ns))

   ("," "Eval from registry and pprint" :eval/registry-pprint)
   ("<RET>" "Eval outer sexp" :eval/outer-sexp))))
