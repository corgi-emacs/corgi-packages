;;; corkey.el --- Keybinding mechanics for Corgi
;;
;; Copyright (C) 2020-2021 Gaiwan GmbH
;;
;; Author: Arne Brasseur <arne@gaiwan.co>
;; Package-Requires: ((use-package) (a) (evil) (which-key))
;;

;;; Commentary:

;; For infor on use, see the Corgi manual.

;;;; Implementation:

;; This code heavily piggiebacks on Evil, and as such it's good to understand
;; where Evil's keybindings live. Evil makes use of "emulation" keymaps via
;; `emulation-mode-map-alists', which have high precedence. They come before
;; minor mode keymaps, before the "local" map (which contains the major-mode
;; bindings), and before the "global" map.
;;
;; `emulation-mode-map-alists' will contain the `evil-mode-map-alist' variable,
;; which gets rebuilt whenever the evil state changes, based on a number of
;; other variables. The main one we are concerned about is
;; `evil-minor-mode-keymaps-alist', which contains per-state per-minor-mode
;; keymaps.
;;
;; In here we create "fake" minor modes for each major mode, as well as adding
;; entries for `corkey-local-mode', our globalized minor mode.

;;; Code:

(require 'use-package)
(require 'seq)

(use-package evil :init (setq evil-want-keybinding nil))
(use-package which-key)
(use-package a)

(defun corkey--ancestor-modes (mode)
  "List of the given mode, plus any of its ancestors

By traversing the 'derived-mode-parent symbol property."
  (cons mode
        (when-let (derived-mode (get mode 'derived-mode-parent))
          (corkey--ancestor-modes derived-mode))))

(defun corkey--set-shadow-mode-vars ()
  "Create shadow-mode variables based on the major-mode

For each major mode Corkey creates a buffer-local variable which
is set to `t' when the major mode is active, or `nil' if it is
not. We treat these as minor modes which shadow the major mode,
and assign key bindings that are specific to a given major mode
to this minor mode instead, so that we don't mutate the
major-mode keymap. This way the bindings can easily be disabled
when corkey-mode is switched off."
  (seq-doseq (mode (corkey--ancestor-modes major-mode))
    (let ((shadow-mode-var (intern (concat "corkey--" (symbol-name mode)))))
      (make-variable-buffer-local shadow-mode-var)
      (set shadow-mode-var corkey-local-mode))))

(define-minor-mode corkey-local-mode
  "Minor mode providing corkey bindings"
  :lighter ""
  :keymap (make-sparse-keymap)
  ;; To have bindings that are specific to a major mode, without actually
  ;; changing that major-mode's mode-map, we fake a minor mode (really just a
  ;; variable) that is true/on when the given major-mode is enabled (it shadows
  ;; the major mode, hence the name). When loading key bindings into evil we
  ;; associate them with this shadow minor mode. This way the corkey bindings
  ;; remain isolated and can easily be toggled.
  (corkey--set-shadow-mode-vars))

(defun corkey-initialize ()
  (unless (and (minibufferp) (not evil-want-minibuffer))
    (corkey-local-mode)))

(define-globalized-minor-mode corkey-mode
  corkey-local-mode
  corkey-initialize)

(defun corkey/-flatten-bindings (state prefix bindings)
  "Takes nested binding definitions as found in corkey-keys.el, and
returns a flat list of (state binding description signal-or-command), e.g.

```
 (visual \"SPC f r\" \"Recently opened files\" :file/open-recent)
 (normal \"SPC f A\" \"Find alternate file\" find-alternate-file)
```"
  (let ((head (car bindings))
        (rest (cdr-safe bindings))
        (states (mapcar #'intern (split-string (symbol-name state) "|"))))
    (cond
     ((symbolp head)
      (seq-mapcat (lambda (b)
                    (corkey/-flatten-bindings head prefix b))
                  rest))
     (t
      (let ((desc (car-safe rest))
            (rest (cdr-safe rest)))
        (if (consp (car rest))
            (append (mapcar (lambda (state)
                              (list state (concat prefix head) desc))
                            states)
                    (seq-mapcat (lambda (b)
                                  (corkey/-flatten-bindings state (concat prefix head " ") b))
                                rest))
          (mapcar (lambda (state)
                    (list state (concat prefix head) desc (car rest)))
                  states)))))))

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

;; This has become a bit of a mess, where we use a mix of different ways of
;; setting key bindings. The good news is that it now works as advertised.
;;
;; - possible to set minor mode keys
;; - possible to set major mode keys
;; - possible to set global (mode-independent) keys
;; - key precedence is correct (minor overrides major overrides global)
;; - Corkey bindings override other bindings (always have preference)
;; - evil state is respected
;; - support a "global" state, i.e. any evil state
;;
;; The bad news is that disabling corkey-mode will no longer diable all bindings
;; that were set up via Corkey, but only the major mode and global bindings.
;; Minor mode bindings will remain, since these are added directly to the
;; minor-mode keymap or global keymap.
;;
;; How are bindings set up?
;; - minor mode + evil state -> evil-define-minor-mode-key
;; - major mode + evil state -> evil-define-minor-mode-key on "shadow" mode (see other comments in this file)
;; - minor or major mode + "global" state -> define-key on the minor-mode-map or shadow mode map
;; - "global" mode -> corgi-local-mode (globalized minor mode) mode map via evil-define-minor-mode-key

(defun corkey/define-key (state mode-sym keys target &optional description)
  "Install a single binding, for a specific Evil STATE and a given
major/minor mode. Note that in the case of major modes this does
not change the major-modes keymap itself, but instead adds the
binding to a \"shadow-mode\" which will always be enabled in
tandem with the major mode. For minor modes we do currently
manipulate the mode's keymap itself.

If STATE is `\'global' then the binding is available regardless
of evil's state.

if MODE-SYM is `\'global' then the binding is available
regardless of the major mode.

When the optional DESCRIPTION is provided then we set up
`which-key' to use this description."
  (let ((mode-var (if (boundp mode-sym)
                      ;; This is for minor modes, in this case we
                      ;; do change the minor mode keymap, instead
                      ;; of the shadow mode keymap, since we don't
                      ;; shadow minor modes.
                      mode-sym
                    (intern (concat "corkey--" (symbol-name mode-sym))))))
    (cond
     ((and (eq 'global state) (eq 'global mode-sym))
      (define-key corkey-local-mode-map (kbd keys) target))
     ((eq 'global state)
      (define-key
       (symbol-value
        (intern (concat (symbol-name mode-var) "-map")))
       (kbd keys)
       target))
     ((eq 'global mode-sym)
      (evil-define-minor-mode-key state 'corkey-local-mode (kbd keys) target))
     (t
      (evil-define-minor-mode-key state mode-var (kbd keys) target))))
  (when description
    (which-key-add-major-mode-key-based-replacements mode-sym keys description)))

(defun corgi/fix-keymap-precedence ()
  "Move `corkey-local-mode' bindings to the end of
`evil-minor-mode-keymaps-alist'. This allows us to override
`global' bindings for specific major modes, e.g. have a different
forward-sexp (`L') in `clojure-mode'. The overriding binding is
set via a shadow minor mode var, which will come before the
`corkey-local-mode' var, which contains our bindings."
  (setq evil-minor-mode-keymaps-alist
        (seq-map
         (lambda (sm)
           (let ((state (car sm))
                 (modes (cdr sm)))
             (cons state
                   (append
                    (seq-remove (lambda (el)
                                  (eq (car el) 'corkey-local-mode))
                                modes)
                    (seq-filter (lambda (el)
                                  (eq (car el) 'corkey-local-mode))
                                modes)))))
         evil-minor-mode-keymaps-alist)))

(defun corkey/setup-keymaps (bindings signals)
  "Take a list of (flattened) Corgkey bindings, and a list of
signals, combines them to find per-state-and-mode bindings, and
installs them in the corresponding evil keymaps, setting up
which-key replacements where available."
  (mapc
   (lambda (binding)
     (pcase-let ((`(,state ,keys ,desc ,target) binding))

       (cond
        ;; Prefixes
        ((not target)
         (which-key-add-key-based-replacements keys desc))

        ;; Signal dispatch
        ((keywordp target)
         (let ((mode-targets (cdr (assoc target signals))))
           (mapc
            (lambda (mode-target)
              (let* ((mode-name (car mode-target))
                     (rest (cdr mode-target)))
                (if (symbolp rest)
                    (corkey/define-key state mode-name keys rest desc)

                  ;; Major-mode specific description
                  (corkey/define-key state mode-name keys (cadr rest) (car rest)))))
            mode-targets)))

        ;; Direct mapping to command
        ((symbolp target)
         (corkey/define-key state 'global keys target desc)))))
   bindings)
  (corgi/fix-keymap-precedence)
  nil)

(defun corkey/-read-file (file-name)
  (with-current-buffer (find-file-noselect file-name)
    (auto-revert-mode 1)
    (goto-char (point-min))
    (read (current-buffer))))

(defun corkey/-locate-file (fname)
  "Look up a Corkey binding or signal file. Should be either a
symbol or a relative file name. Will first check the
user-emacs-directory, falling back to locating the file on
emacs's library path.

```
(corkey/-locate-file 'corgi-bindings)
(corkey/-locate-file \"corgi-bindings.el\")
```
"
  (cond
   ((symbolp fname)
    (corkey/-locate-file (concat (symbol-name fname) ".el")))
   ((file-exists-p (expand-file-name fname user-emacs-directory))
    (expand-file-name fname user-emacs-directory))
   (t (locate-library fname))))

(defun corkey/-load-bindings (binding-files)
  "Load one or more BINDING_FILES, a list of symbols or relative
file names, see [[corkey/-locate-file]], and return the fully
merged and flattened list of bindings defined therein."
  (seq-mapcat
   (lambda (f)
     (thread-last f
       corkey/-locate-file
       corkey/-read-file
       (corkey/-flatten-bindings 'normal "")))
   binding-files))

(defun corkey/-load-signals (signal-files)
  "Load one or more SIGNAL_FILES, a list of symbols or relative
file names, see [[corkey/-locate-file]], and return the fully
merged and flattened list of signals defined therein."
  (seq-reduce
   #'corkey/-flatten-signals
   (mapcar (lambda (f)
             (corkey/-read-file (corkey/-locate-file f)))
           signal-files)
   nil))

(defun corkey/install-bindings (&optional binding-files signal-files)
  (interactive)
  (let* ((binding-files (or binding-files 'corgi-keys))
         (signal-files (or signal-files 'corgi-signals))

         (binding-files (if (listp binding-files)
                            binding-files
                          (list binding-files)))
         (signal-files (if (listp signal-files)
                           signal-files
                         (list signal-files))))

    (corkey/setup-keymaps
     (corkey/-load-bindings binding-files)
     (corkey/-load-signals signal-files))))

(provide 'corkey)



;;; corkey.el ends here
