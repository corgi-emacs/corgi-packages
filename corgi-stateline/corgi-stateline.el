;;; corgi-stateline.el --- Change the background color of the modeline based on the evil state
;;
;; Filename: corgi-stateline.el
;; Package-Requires: ()
;;

;;; Code:

(defcustom corgi-stateline-normal-fg "black"
  "Foreground color of the modeline in evil normal state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-normal-bg "gray"
  "Background color of the modeline in evil normal state"
  :type 'color
  :group 'corgi)

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

(defvar corgi-stateline-remap-cookie nil
  "Cookie used to hold reference to face remapping.")

(defun corgi-stateline/map-mode-line-face (fg bg)
  (when corgi-stateline-remap-cookie
    (face-remap-remove-relative corgi-stateline-remap-cookie))
  (setq corgi-stateline-remap-cookie (face-remap-add-relative
                                      'mode-line :foreground fg :background bg)))

(defun corgi-stateline/enter-normal-state ()
  (corgi-stateline/map-mode-line-face corgi-stateline-normal-fg
                                      corgi-stateline-normal-bg))

(defun corgi-stateline/enter-motion-state ()
  (corgi-stateline/map-mode-line-face corgi-stateline-motion-fg
                                      corgi-stateline-motion-bg))

(defun corgi-stateline/enter-insert-state ()
  (corgi-stateline/map-mode-line-face corgi-stateline-insert-fg
                                      corgi-stateline-insert-bg))

(defun corgi-stateline/enter-visual-state ()
  (corgi-stateline/map-mode-line-face corgi-stateline-visual-fg
                                      corgi-stateline-visual-bg))

(defun corgi-stateline/enter-emacs-state ()
  (corgi-stateline/map-mode-line-face corgi-stateline-emacs-fg
                                      corgi-stateline-emacs-bg))

(defun corgi-stateline/turn-on ()
  (message "corgi-stateline adding hooks")
  (add-hook 'evil-normal-state-entry-hook #'corgi-stateline/enter-normal-state)
  (add-hook 'evil-motion-state-entry-hook #'corgi-stateline/enter-motion-state)
  (add-hook 'evil-insert-state-entry-hook #'corgi-stateline/enter-insert-state)
  (add-hook 'evil-visual-state-entry-hook #'corgi-stateline/enter-visual-state)
  (add-hook 'evil-emacs-state-entry-hook #'corgi-stateline/enter-emacs-state))

(defun corgi-stateline/turn-off ()
  (message "corgi-stateline Removing hooks")
  (remove-hook 'evil-normal-state-entry-hook #'corgi-stateline/enter-normal-state)
  (remove-hook 'evil-motion-state-entry-hook #'corgi-stateline/enter-motion-state)
  (remove-hook 'evil-insert-state-entry-hook #'corgi-stateline/enter-insert-state)
  (remove-hook 'evil-visual-state-entry-hook #'corgi-stateline/enter-visual-state)
  (remove-hook 'evil-emacs-state-entry-hook #'corgi-stateline/enter-emacs-state)
  (when corgi-stateline-remap-cookie
    (face-remap-remove-relative corgi-stateline-remap-cookie)
    (setq corgi-steteline-remap-cookie nil)))

(define-minor-mode corgi-stateline-mode
  "Toggle corgi-stateline-mode.

When enabled, this mode will change the color of the mode line
based on the current evil editing state."
  :init-value nil
  :global t
  :lighter " StLi"
  (if (default-value 'corgi-stateline-mode)
      (corgi-stateline/turn-on)
    (corgi-stateline/turn-off)))

(provide 'corgi-stateline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-stateline.el ends here
