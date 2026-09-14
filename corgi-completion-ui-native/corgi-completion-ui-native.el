;;; corgi-completion-ui-native.el --- Zero-dependency completion UI for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-completion-ui-native.el
;; Package-Requires: ()
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Alternative to `corgi-completion-ui' that uses only Emacs built-ins, no
;; dependencies. Based on `icomplete' / `fido' for minibuffer completions, and
;; `completion-preview-mode' for in-buffer completions.
;;
;; This is an experimental package, the exact stack, and the name of the
;; package, will likely still change. In particular this may become the corgi
;; default, with additional packages (like `corgi-completion-ui-vertico' for
;; popular third party completion stacks).
;;
;; See `corgi-completion-ui-native-signals.el' for binding overrides

;;; Code:

;; Vertical, ido-like minibuffer completion.  `fido-vertical-mode' enables
;; `fido-mode' (flex matching, case-insensitive) plus the vertical display.
(require 'icomplete)
(fido-vertical-mode 1)

;; Show a "»" next to the selected candidate (Emacs 31+)
(when (boundp 'icomplete-vertical-render-prefix-indicator)
  (setq icomplete-vertical-render-prefix-indicator t))

;; In-buffer completion previews (Emacs 30+).  Guarded so that Emacs 29 users
;; can load this file too; there `completion-at-point' remains the way to
;; complete in a buffer.
(when (fboundp 'completion-preview-mode)
  (global-completion-preview-mode 1))

;; For project-level commands like `project-find-file', `project-search', etc.
;; See the accompanying bindings file.
(require 'project)

(with-eval-after-load 'corkey
  (corkey-register-signal-file 'corgi-completion-ui-native-signals))

(provide 'corgi-completion-ui-native)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-completion-ui-native.el ends here
