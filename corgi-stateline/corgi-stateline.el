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

(defcustom corgi-stateline-normal-bg "orange"
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

(defun corgi-stateline/enter-normal-state ()
  (set-face-foreground 'mode-line corgi-stateline-normal-fg)
  (set-face-background 'mode-line corgi-stateline-normal-bg))

(defun corgi-stateline/enter-motion-state ()
  (set-face-foreground 'mode-line corgi-stateline-motion-fg)
  (set-face-background 'mode-line corgi-stateline-motion-bg))

(defun corgi-stateline/enter-insert-state ()
  (set-face-foreground 'mode-line corgi-stateline-insert-fg)
  (set-face-background 'mode-line corgi-stateline-insert-bg))

(defun corgi-stateline/enter-visual-state ()
  (set-face-foreground 'mode-line corgi-stateline-visual-fg)
  (set-face-background 'mode-line corgi-stateline-visual-bg))

(defun corgi-stateline/enter-emacs-state ()
  (set-face-foreground 'mode-line corgi-stateline-emacs-fg)
  (set-face-background 'mode-line corgi-stateline-emacs-bg))

(add-hook 'evil-normal-state-entry-hook #'corgi-stateline/enter-normal-state)
(add-hook 'evil-motion-state-entry-hook #'corgi-stateline/enter-motion-state)
(add-hook 'evil-insert-state-entry-hook #'corgi-stateline/enter-insert-state)
(add-hook 'evil-visual-state-entry-hook #'corgi-stateline/enter-visual-state)
(add-hook 'evil-emacs-state-entry-hook #'corgi-stateline/enter-emacs-state)

(provide 'corgi-stateline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-stateline.el ends here
