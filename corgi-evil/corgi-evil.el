;;; corgi-evil.el --- Evil configuration for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-evil.el
;; Package-Requires: ((use-package) (diminish) (evil) (evil-cleverparens) (evil-collection) (evil-surround) (smartparens))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'use-package)

(use-package evil
  :init
  ;; Prevent evil from setting additional keybindings, since we have
  ;; `evil-collection' for that instead
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  ;; Make sure we can redo after undo. We have `undo-redo' since Emacs 28, so
  ;; use that instead of a package like undo-fu
  (evil-set-undo-system 'undo-redo)
  ;; Keep Emacs-style cursor behavior, not vim style "jump back" and EOL
  ;; behavior. If you don't like these, change them back after loading `corgi-evil'
  (setq evil-move-cursor-back nil
        evil-move-beyond-eol t)
  (setq
   ;; Corgi used to set this to `t' to get more fine-grained undo, since other
   ;; big evil-based configs leave this as `nil' we're evaluating doing the
   ;; same.
   ;; evil-want-fine-undo t
   evil-mode-line-format 'before
   evil-normal-state-cursor '(box "orange")
   evil-insert-state-cursor '(box "green")
   evil-visual-state-cursor '(box "#F86155")
   evil-emacs-state-cursor  '(box "purple")))

(use-package evil-collection
  :after (evil)
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

;; TODO move to corgi-lisp or similar
(with-eval-after-load 'evil
  ;; A sexp is characterwise and never linewise.  The default `exclusive' and
  ;; `inclusive' types make `evil-delete' reclassify a delete as linewise when
  ;; the motion starts at the beginning of a line and ends on a line boundary,
  ;; which breaks e.g. `dL' followed by `p'.  We use a custom identity type so
  ;; the range is left characterwise as-is.

  ;; Prevent evil-motion-state from shadowing previous/next sexp
  (require 'evil-maps)
  (define-key evil-motion-state-map "L" nil)
  (define-key evil-motion-state-map "M" nil)

  (evil-define-type corgi-sexp
    "A sexp.  Characterwise and never linewise.")

  (evil-define-motion corgi-forward-sexp (count)
    "Move forward by a sexp, characterwise and never linewise."
    :jump t
    :type corgi-sexp
    (when (evil-eolp) (forward-char))
    (sp-forward-sexp (or count 1)))

  (evil-define-motion corgi-backward-sexp (count)
    "Move backward by a sexp, characterwise and never linewise."
    :jump t
    :type corgi-sexp
    (sp-backward-sexp (or count 1))))

;; Always move to evil default mode when switching buffers
(defvar corgi-evil--last-buffer
  nil
  "The last current buffer.")

(defun corgi-evil--on-buffer-change ()
  (unless (or (minibufferp)
              (eq (current-buffer) corgi-evil--last-buffer))
    (setq corgi-evil--last-buffer (current-buffer))
    (evil-change-to-initial-state)))

(add-hook 'post-command-hook #'corgi-evil--on-buffer-change)

;; silence byte compiler
(require 'evil)
(require 'evil-core)
(require 'evil-collection)
(require 'smartparens)

(provide 'corgi-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-evil.el ends here
