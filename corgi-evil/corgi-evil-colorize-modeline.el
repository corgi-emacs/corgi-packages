;;; corgi-evil-colorize-modeline.el --- Change the background color of the modeline based on the evil state -*- lexical-binding: t -*-
;;
;; Filename: corgi-evil-colorize-modeline.el
;; Package-Requires: ((evil))
;;

;;; Code:

(require 'face-remap)
(require 'evil)

(defcustom corgi-evil-colorize-modeline-motion-fg "black"
  "Foreground color of the modeline in evil motion state"
  :type 'color
  :group 'corgi)

(defcustom corgi-evil-colorize-modeline-motion-bg "orangered"
  "Background color of the modeline in evil motion state"
  :type 'color
  :group 'corgi)

(defcustom corgi-evil-colorize-modeline-insert-fg "black"
  "Foreground color of the modeline in evil insert state"
  :type 'color
  :group 'corgi)

(defcustom corgi-evil-colorize-modeline-insert-bg "green"
  "Background color of the modeline in evil insert state"
  :type 'color
  :group 'corgi)

(defcustom corgi-evil-colorize-modeline-visual-fg "white"
  "Foreground color of the modeline in evil visual state"
  :type 'color
  :group 'corgi)

(defcustom corgi-evil-colorize-modeline-visual-bg "royalblue1"
  "Background color of the modeline in evil visual state"
  :type 'color
  :group 'corgi)

(defcustom corgi-evil-colorize-modeline-emacs-fg "white"
  "Foreground color of the modeline in evil emacs state"
  :type 'color
  :group 'corgi)

(defcustom corgi-evil-colorize-modeline-emacs-bg "slateblue3"
  "Background color of the modeline in evil emacs state"
  :type 'color
  :group 'corgi)

(defvar corgi-evil-colorize-modeline-mode-line-face
  (if (facep 'mode-line-active) 'mode-line-active 'mode-line)
  "Face to remap for the selected window's mode line.
Emacs 29 split the mode line into `mode-line-active' and
`mode-line-inactive'; remapping `mode-line' no longer has any
effect on the actual mode lines.")

(defvar-local corgi-evil-colorize-modeline-remap-cookie nil
  "Cookie used to hold reference to face remapping.")

(defun corgi-evil-colorize-modeline/map-mode-line-face (fg bg)
  (when corgi-evil-colorize-modeline-remap-cookie
    (face-remap-remove-relative corgi-evil-colorize-modeline-remap-cookie))
  (setq corgi-evil-colorize-modeline-remap-cookie (face-remap-add-relative
                                      corgi-evil-colorize-modeline-mode-line-face
                                      :foreground fg :background bg)))

(defun corgi-evil-colorize-modeline/unmap-mode-line-face ()
  (when corgi-evil-colorize-modeline-remap-cookie
    (face-remap-remove-relative corgi-evil-colorize-modeline-remap-cookie)
    (setq corgi-evil-colorize-modeline-remap-cookie nil)))

(defun corgi-evil-colorize-modeline/update-mode-line-face ()
  (cond
   ((eq evil-state 'normal)
    (corgi-evil-colorize-modeline/unmap-mode-line-face))

   ((eq evil-state 'motion)
    (corgi-evil-colorize-modeline/map-mode-line-face corgi-evil-colorize-modeline-motion-fg
                                        corgi-evil-colorize-modeline-motion-bg))

   ((eq evil-state 'insert)
    (corgi-evil-colorize-modeline/map-mode-line-face corgi-evil-colorize-modeline-insert-fg
                                        corgi-evil-colorize-modeline-insert-bg))

   ((eq evil-state 'visual)
    (corgi-evil-colorize-modeline/map-mode-line-face corgi-evil-colorize-modeline-visual-fg
                                        corgi-evil-colorize-modeline-visual-bg))

   ((eq evil-state 'emacs)
    (corgi-evil-colorize-modeline/map-mode-line-face corgi-evil-colorize-modeline-emacs-fg
                                        corgi-evil-colorize-modeline-emacs-bg))

   (t
    (corgi-evil-colorize-modeline/unmap-mode-line-face))))

(defconst corgi-evil-colorize-modeline--state-entry-hooks
  '(evil-normal-state-entry-hook
    evil-motion-state-entry-hook
    evil-insert-state-entry-hook
    evil-visual-state-entry-hook
    evil-emacs-state-entry-hook)
  "Evil state entry hooks that update the mode line face.")

(defun corgi-evil-colorize-modeline/turn-on ()
  (dolist (hook corgi-evil-colorize-modeline--state-entry-hooks)
    (add-hook hook #'corgi-evil-colorize-modeline/update-mode-line-face nil t))
  (corgi-evil-colorize-modeline/update-mode-line-face))

(defun corgi-evil-colorize-modeline/turn-off ()
  (dolist (hook corgi-evil-colorize-modeline--state-entry-hooks)
    (remove-hook hook #'corgi-evil-colorize-modeline/update-mode-line-face t))
  (corgi-evil-colorize-modeline/unmap-mode-line-face))

(define-minor-mode corgi-evil-colorize-modeline-mode
  "Toggle corgi-evil-colorize-modeline-mode.

When enabled, this mode will change the color of the mode line
based on the current evil editing state."
  :init-value nil
  :group 'corgi
  (if corgi-evil-colorize-modeline-mode
      (corgi-evil-colorize-modeline/turn-on)
    (corgi-evil-colorize-modeline/turn-off)))

(define-globalized-minor-mode global-corgi-evil-colorize-modeline-mode
  corgi-evil-colorize-modeline-mode
  (lambda ()
    (unless (minibufferp)
      (corgi-evil-colorize-modeline-mode 1)))
  :group 'corgi)

(provide 'corgi-evil-colorize-modeline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-evil-colorize-modeline.el ends here
