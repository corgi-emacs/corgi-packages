
emulation-mode-map-alists

;; emulation-mode-map-alists is a variable defined in ‘src/keymap.c’.
;; Its value is (company-emulation-alist evil-mode-map-alist)

;; Probably introduced at or before Emacs version 22.1.

;; Documentation:
;; List of keymap alists to use for emulation modes.
;; It is intended for modes or packages using multiple minor-mode keymaps.
;; Each element is a keymap alist just like ‘minor-mode-map-alist’, or a
;; symbol with a variable binding which is a keymap alist, and it is used
;; the same way.  The "active" keymaps in each alist are used before
;; ‘minor-mode-map-alist’ and ‘minor-mode-overriding-map-alist’.

(info "(elisp)Active Keymaps")

;; Usually, the active keymaps are:
;; (i) the keymap specified by the keymap property,
;; (ii) the keymaps of enabled minor modes,
;; (iii) the current buffer’s local keymap,
;; (iv) the global keymap, in that order.
;; Emacs searches for each input key sequence in all these keymaps.

;; keymaps specified by enabled minor modes.
;; These keymaps, if any, are specified by the variables:
emulation-mode-map-alists
;; => (evil-mode-map-alist company-emulation-alist)

minor-mode-overriding-map-alist
;; => nil
minor-mode-map-alist

evil-mode-map-alist
;; buffer local, created / populated by
(evil-normalize-keymaps)
evil-minor-mode-keymaps-alist
"Create a buffer-local value for `evil-mode-map-alist'.
This is a keymap alist, determined by the current state
\(or by STATE if specified)."


evil-mode-map-alist

(progn
  (evil-define-key 'normal keymap (kbd keys) cmd)
  (evil-define-key 'motion keymap (kbd keys) cmd))

(let ((mode 'emacs-lisp-mode))
  (list (boundp mode) (symbol-value mode)))

major-mode

(dolist (map (current-active-maps))
  (when (evil-get-auxiliary-keymap map 'normal)
    (setq xxx (evil-get-auxiliary-keymap map 'normal))
    ))


emulation-mode-map-alists

;; Types of bindings
;; - for all major modes, but for specific states
;; - for specific major mode (and specific state(s))
;; - for all major modes, in emacs state
;; - for specific major mode, in emacs state

;; So... really collapses down to per-major-mode or not, but always
;; per state, so stick to evil mechanisms.

;; Evil options
;; - can be anything in (current-active-maps), rely on evil-normalize-keymaps

(evil-define-minor-mode-key
  'normal
  'corkey--emacs-lisp
  (kbd "C-z")
  (lambda () (interactive) (message "YOOOOWWW")))

(evil-get-minor-mode-keymap 'normal 'corkey-local-mode)
(evil-get-minor-mode-keymap 'normal 'corkey--emacs-lisp-mode)
(evil-get-minor-mode-keymap 'normal 'corkey--clojure-mode)
(setq evil-minor-mode-keymaps-alist nil)
(local-variable-p 'evil-minor-mode-keymaps-alist)
evil-minor-mode-keymaps-alist
(setq corkey--emacs-lisp nil)

(kbd "C-z x y <SPC>")

(current-minor-mode-maps)

(current-active-maps)

(current-minor-mode-maps)

(local-variable-p 'minor-mode-alist)

(seq-filter (lambda (m) (when (boundp m) (symbol-value m)))
            minor-mode-list)

minor-mode-alist

define-minor-mode

(defun corkey/-flatten-bindings2 (mode prefix bindings)
  (let ((head (car bindings))
        (rest (cdr-safe bindings)))
    (if (symbolp head)
        (seq-mapcat (lambda (b)
                      (corkey/-flatten-bindings2 head prefix b))
                    rest)
      (let ((desc (car-safe rest))
            (rest (cdr-safe rest)))
        (if (consp (car rest))
            (cl-list* (list mode (concat prefix head) desc)
                      (seq-mapcat (lambda (b)
                                    (corkey/-flatten-bindings2 mode (concat prefix head " ") b))
                                  rest))
          (list (list mode (concat prefix head) desc (car rest))))))))

(use-package a)

(defun corkey/-flatten-signals (acc signals)
  (seq-reduce
   (lambda (acc mode-spec)
     (let ((mode (car mode-spec))
           (mapping (cadr mode-spec)))
       (seq-reduce
        (lambda (acc signal-command)
          (a-assoc-in acc (list (car signal-command) mode) (cadr signal-command)))
        (seq-partition mapping 2)
        acc)))
   signals
   acc))

(defun corkey/setup-keymaps (bindings signals)
  (mapc
   (lambda (binding)
     (pcase-let ((`(,mode ,keys ,desc ,target) binding))
       (cond
        ((not target))
        ((keywordp target)
         (let ((mode-targets (cdr (assoc target signals))))
           (mapc
            (lambda (mode-target)
              (let ((major-mode (car mode-target))
                    (shadow-mode-var (intern (concat "corkey--" (symbol-name major-mode))))
                    (rest (cdr mode-target)))
                (if (symbolp rest)
                    (evil-define-minor-mode-key mode shadow-mode-var (kbd keys) rest)
                  (evil-define-minor-mode-key mode shadow-mode-var (kbd keys) (cadr rest))
                  (which-key-add-key-based-replacements keys (car rest)))))
            mode-targets)))
        ((symbolp target)
         (evil-define-minor-mode-key mode 'corkey-local-mode (kbd keys) target)
         (which-key-add-key-based-replacements keys desc)))))
   bindings)
  nil)

(pcase-let ((`(,major-mode ,arg1 ,arg2) '(1 . 2)))
  arg2)
(normal ("TAB"))

(concat "" ("TAB" "Indent" :format/tab-indent))

(corkey/setup-keymaps
 (corkey/-flatten-bindings2
  'normal
  ""
  (corkey/-read-file
   "/home/arne/github/lambdaisland/corgi-packages/corgi-keys.el"))

 (corkey/-flatten-signals
  nil
  (corkey/-read-file
   "/home/arne/github/lambdaisland/corgi-packages/corgi-signals.el")))

which-key-add-key-based-replacements
which-key-add-major-mode-key-based-replacements
provided-mode-derived-p

(symbol-plist 'corkey-mode)
(boundp 'corkey-mode)
corkey-mode
