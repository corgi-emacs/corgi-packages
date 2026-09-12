;;; corgi-stateline.el --- Obsolete shim for corgi-evil-colorize-modeline -*- lexical-binding: t -*-
;;
;; Filename: corgi-stateline.el
;; Package-Requires: ((corgi-evil))
;;

;;; Commentary:
;;
;; This functionality has moved to `corgi-evil-colorize-modeline', which lives
;; in the `corgi-evil' package.

;;; Code:

(require 'corgi-evil-colorize-modeline)

(defvar corgi-stateline--warned nil
  "Whether the deprecation warning has already been emitted this session.")

(defun corgi-stateline--warn (obsolete replacement)
  "Emit a deprecation warning for OBSOLETE, suggesting REPLACEMENT.
Only warns once per session."
  (unless corgi-stateline--warned
    (setq corgi-stateline--warned t)
    (display-warning 'corgi-stateline (format "`%s' is obsolete, use `%s' instead" obsolete replacement) :warning)))

(defun corgi-stateline-mode (&optional arg)
  "Deprecated, use `corgi-evil-colorize-modeline-mode'"
  (interactive "P")
  (corgi-stateline--warn 'corgi-stateline-mode 'corgi-evil-colorize-modeline-mode)
  (corgi-evil-colorize-modeline-mode arg))

(defun global-corgi-stateline-mode (&optional arg)
  "Deprecated, use `global-corgi-evil-colorize-modeline-mode'"
  (interactive "P")
  (corgi-stateline--warn 'global-corgi-stateline-mode 'global-corgi-evil-colorize-modeline-mode)
  (global-corgi-evil-colorize-modeline-mode arg))

(make-obsolete 'corgi-stateline-mode 'corgi-evil-colorize-modeline-mode "0.1")
(make-obsolete 'global-corgi-stateline-mode 'global-corgi-evil-colorize-modeline-mode "0.1")
(define-obsolete-variable-alias 'corgi-stateline-mode 'corgi-evil-colorize-modeline-mode "0.1")
(define-obsolete-variable-alias 'global-corgi-stateline-mode 'global-corgi-evil-colorize-modeline-mode "0.1")
(define-obsolete-variable-alias 'corgi-stateline-motion-fg 'corgi-evil-colorize-modeline-motion-fg "0.1")
(define-obsolete-variable-alias 'corgi-stateline-motion-bg 'corgi-evil-colorize-modeline-motion-bg "0.1")
(define-obsolete-variable-alias 'corgi-stateline-insert-fg 'corgi-evil-colorize-modeline-insert-fg "0.1")
(define-obsolete-variable-alias 'corgi-stateline-insert-bg 'corgi-evil-colorize-modeline-insert-bg "0.1")
(define-obsolete-variable-alias 'corgi-stateline-visual-fg 'corgi-evil-colorize-modeline-visual-fg "0.1")
(define-obsolete-variable-alias 'corgi-stateline-visual-bg 'corgi-evil-colorize-modeline-visual-bg "0.1")
(define-obsolete-variable-alias 'corgi-stateline-emacs-fg 'corgi-evil-colorize-modeline-emacs-fg "0.1")
(define-obsolete-variable-alias 'corgi-stateline-emacs-bg 'corgi-evil-colorize-modeline-emacs-bg "0.1")

(provide 'corgi-stateline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-stateline.el ends here
