
emulation-mode-map-alist

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

Types of bindings
- for all major modes, but for specific states
- for specific major mode (and specific state(s))
- for all major modes, in emacs state
- for specific major mode, in emacs state

So... really collapses down to per-major-mode or not, but always
per state, so stick to evil mechanisms.

Evil options
- can be anything in (current-active-maps), rely on evil-normalize-keymaps

(evil-define-minor-mode-key
  'normal
  'corkey--emacs-lisp
  (kbd "C-z")
  (lambda () (interactive) (message "YOOOOWWW")))

(evil-get-minor-mode-keymap 'normal 'corkey-mode)
(evil-get-minor-mode-keymap 'normal 'corkey--emacs-lisp)
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
