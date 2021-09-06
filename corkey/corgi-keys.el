;;; -*- no-byte-compile: t -*-

;; <def>    = '(' <key> <doc> <target> ')' | '(' <prefix> <def> + ')'
;; <target> = <signal> | <command>
;; <prefix> = <state> | <key>
;; <state>  = 'normal' | 'insert' | 'visual' | 'emacs' | 'motion'
;; <key>     := stringp
;; <doc>     := stringp
;; <signal>  := keywordp
;; <command> := symbolp

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
 ("<M-down>" "Expand region" er/contract-region)

 ("gc" "Comment region" comment-region)
 ("gC" "Uncomment region" uncomment-region)

 ("C-x" ("C-e" "Eval form before cursor" :eval/last-sexp))

 ("SPC" "Global leader key"
  ("b" "Buffer commands"
   ("b" "Switch buffer" ivy-switch-buffer)
   ("d" "Kill current buffer" kill-this-buffer)
   ("k" "Pick & kill" kill-buffer)
   ("l" "List buffers" list-buffers)
   ("r" "Rename buffer" rename-buffer)
   ("w" "Toggle read-only" read-only-mode))

  ("f" "File commands"
   ("f" "Find file" :file/open)
   ("s" "Save file" :file/save)
   ("S" "Save all" :file/save-all)
   ("r" "Recently opened files" :file/open-recent)
   ("A" "Find alternate file" find-alternate-file)
   ("e" "Emacs files"
    ("i" "Open init.el" corgi/open-init-el)
    ("b" "Open bindings file" corgi/open-bindings)
    ("u" "Open user config" corgi/open-user-config)))

  ("s" "Search commands"
   ("s" "Search in buffer" :buffer/incremental-search))

  ("p" "Project"
   ("f" "Find file" :project/open-file)
   ("p" "Switch project" :project/switch)
   ("k" "Kill buffers" :project/kill)
   ("s" "Search in project" :project/incremental-search))

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
   ("a" "Toggle aggressive indent mode" aggressive-indent-mode)
   ("l" "Toggle line numbers" linum-mode))

  ("x" "Text editing"
   ("t" "Transpose sexps" transpose-sexps)
   ("s" "Splice backwards" sp-splice-sexp-killing-backward))

  ("SPC" "Execute command (M-x)" :command/execute)
  ("u" "Universal prefix" universal-argument)
  ("TAB" "Switch to previous buffer" corgi/switch-to-previous-buffer)
  ("1" "Select window 1" winum-select-window-1)
  ("2" "Select window 2" winum-select-window-2)
  ("3" "Select window 3" winum-select-window-3)
  ("4" "Select window 4" winum-select-window-4)
  ("5" "Select window 5" winum-select-window-5)
  ("6" "Select window 6" winum-select-window-6)
  ("7" "Select window 7" winum-select-window-7)
  ("8" "Select window 8" winum-select-window-8)
  ("9" "Select window 9" winum-select-window-9)
  ("0" "Select window 10" winum-select-window-10))

 ("," "Project specific leader key"

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

  ("s" "REPL"
   ("s" "Toggle REPL" :repl/toggle)
   ("q" "Quit current REPL" :repl/quit)
   ("Q" "Quit all active REPLs" :repl/quit-all)
   ("o" "Switch to Other REPL" :repl/other)
   ("c" "Connect to REPL" :repl/connect)
   ("l" "Clear REPL" :repl/clear)
   ("n" "Set namespace" :repl/set-ns)
   ("m" "Toggle message logging" :repl/toggle-message-logging))

  ("g" "Go places"
   ("g" "Go to definition" :jump/definition)
   ("b" "Go back" :jump/back)
   ("n" "Go to namespace" :jump/ns)
   ("t" "Go to test/implemenentation" projectile-toggle-between-implementation-and-test))

  ("l" "Link to REPL"
   ("p" "Link with project" sesman-link-with-project)
   ("b" "Link with buffer" sesman-link-with-buffer)
   ("d" "Link with directory" sesman-link-with-directory)
   ("l" "Link least specific" sesman-link-with-least-specific)
   ("u" "Unlink" sesman-unlink))

  ("j" "Jack-in"
   ("j" "Jack in" :repl/jack-in)
   ("o" "Jack in other" :repl/jack-in-other)
   ("a" "Jack in all" :repl/jack-in-all))

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

  ("," "Eval from registry and pprint" :eval/registry-pprint)
  ("<RET>" "Eval outer sexp" :eval/outer-sexp)))
