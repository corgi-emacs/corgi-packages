;;; corgi-stateline.el --- Change the background color of the modeline based on the evil state -*- lexical-binding: t -*-
;;
;; Filename: corgi-stateline.el
;; Package-Requires: ()
;;

;;; Code:

(require 'face-remap)

(defcustom corgi-stateline-motion-fg "black"
  "Foreground color of the modeline in evil motion state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-motion-bg "orangered"
  "Background color of the modeline in evil motion state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-insert-fg "black"
  "Foreground color of the modeline in evil insert state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-insert-bg "green"
  "Background color of the modeline in evil insert state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-visual-fg "white"
  "Foreground color of the modeline in evil visual state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-visual-bg "royalblue1"
  "Background color of the modeline in evil visual state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-emacs-fg "white"
  "Foreground color of the modeline in evil emacs state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-emacs-bg "slateblue3"
  "Background color of the modeline in evil emacs state"
  :type 'color
  :group 'corgi)

(defvar-local corgi-stateline-remap-cookie nil
  "Cookie used to hold reference to face remapping.")

(defun corgi-stateline/map-mode-line-face (fg bg)
  (when corgi-stateline-remap-cookie
    (face-remap-remove-relative corgi-stateline-remap-cookie))
  (setq corgi-stateline-remap-cookie (face-remap-add-relative
                                      'mode-line :foreground fg :background bg)))

(defun corgi-stateline/unmap-mode-line-face ()
  (when corgi-stateline-remap-cookie
    (face-remap-remove-relative corgi-stateline-remap-cookie)
    (setq corgi-stateline-remap-cookie nil)))

(defun corgi-stateline/update-mode-line-face ()
  (cond
   ((eq evil-state 'normal)
    (corgi-stateline/unmap-mode-line-face))

   ((eq evil-state 'motion)
    (corgi-stateline/map-mode-line-face corgi-stateline-motion-fg
                                        corgi-stateline-motion-bg))

   ((eq evil-state 'insert)
    (corgi-stateline/map-mode-line-face corgi-stateline-insert-fg
                                        corgi-stateline-insert-bg))

   ((eq evil-state 'visual)
    (corgi-stateline/map-mode-line-face corgi-stateline-visual-fg
                                        corgi-stateline-visual-bg))

   ((eq evil-state 'emacs)
    (corgi-stateline/map-mode-line-face corgi-stateline-emacs-fg
                                        corgi-stateline-emacs-bg))))

(defun corgi-stateline/turn-on ()
  (add-hook 'evil-normal-state-entry-hook #'corgi-stateline/update-mode-line-face)
  (add-hook 'evil-motion-state-entry-hook #'corgi-stateline/update-mode-line-face)
  (add-hook 'evil-insert-state-entry-hook #'corgi-stateline/update-mode-line-face)
  (add-hook 'evil-visual-state-entry-hook #'corgi-stateline/update-mode-line-face)
  (add-hook 'evil-emacs-state-entry-hook #'corgi-stateline/update-mode-line-face)
  (corgi-stateline/update-mode-line-face))

(defun corgi-stateline/turn-off ()
  (add-hook 'evil-normal-state-entry-hook #'corgi-stateline/update-mode-line-face)
  (add-hook 'evil-motion-state-entry-hook #'corgi-stateline/update-mode-line-face)
  (add-hook 'evil-insert-state-entry-hook #'corgi-stateline/update-mode-line-face)
  (add-hook 'evil-visual-state-entry-hook #'corgi-stateline/update-mode-line-face)
  (add-hook 'evil-emacs-state-entry-hook #'corgi-stateline/update-mode-line-face)
  (corgi-stateline/unmap-mode-line-face))

(define-minor-mode corgi-stateline-mode
  "Toggle corgi-stateline-mode.

When enabled, this mode will change the color of the mode line
based on the current evil editing state."
  :init-value nil
  :group 'corgi
  (if corgi-stateline-mode
      (corgi-stateline/turn-on)
    (corgi-stateline/turn-off)))

(define-globalized-minor-mode global-corgi-stateline-mode
  corgi-stateline-mode
  (lambda ()
    (unless (minibufferp)
      (corgi-stateline-mode 1)))
  :group 'corgi)

(provide 'corgi-stateline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-stateline.el ends here
