;;; corgi-cider-connection-indicator.el --- CIDER extensions and patches -*- lexical-binding: t -*-
;;
;; Filename: corgi-cider-connection-indicator.el
;; Package-Requires: ((cider) (projectile))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cider)
(require 'projectile)
(require 'seq)
(require 'cl-lib)
(require 's)

(defun corgi/cider-modeline-info ()
  "Generate a string with background/foreground color properties
that can act as a modeline indicator of which REPLs a given
Clojure buffer is currently connected to."
  (if (not (derived-mode-p 'clojure-mode))
      ""
    (let ((source-project-name (projectile-project-name)))
      (if-let* ((repls (ignore-errors (cider-repls (cider-repl-type-for-buffer)))))
          (thread-last
            repls
            (seq-map
             (lambda (repl)
               (with-current-buffer repl
                 (if (equal (buffer-name repl) "*babashka-repl*")
                     (propertize "bb" 'face '( :background "green"
                                               :foreground "black"))
                   (let ((info (concat
                                (when-let ((repl-project-name (cider--project-name nrepl-project-dir)))
                                  (when (not (equal repl-project-name source-project-name))
                                    (concat ":" repl-project-name)))
                                (pcase (plist-get nrepl-endpoint :host)
                                  ("localhost" "")
                                  ("127.0.0.1" "")
                                  (x (concat ":" x)))
                                ;;(format ":%d" (plist-get nrepl-endpoint :port))
                                )))
                     (cl-case cider-repl-type
                       (clj (propertize (concat "clj" info) 'face '( :background "#5881D8"
                                                                     :foreground "white")))
                       (cljs (propertize (concat "cljs" info) 'face '( :background "#f7df1e"
                                                                       :foreground "black")))
                       (pending-cljs (propertize (concat "pending-cljs" info) 'face '( :background "#f7df1e"
                                                                                       :foreground "black")))))))))
            (s-join " "))
        (propertize "<not connected>" 'face '( :background "red"
                                               :foreground "white"))))))

(defvar corgi/cider-connection-indicator-entry
  '(:eval (concat "  " (corgi/cider-modeline-info) " ")))

(defun corgi/-tree-remove-element (tree element)
  (seq-mapcat
   (lambda (e)
     (cond
      ((equal e element) nil)
      ((listp e) (list (corgi/-tree-remove-element e element)))
      (t (list e))))
   tree))

(defun corgi/-tree-add-element (tree match element &optional prepend)
  (seq-mapcat
   (lambda (e)
     (cond
      ((equal e match) (if prepend (list element e) (list e element)))
      ((listp e) (list (corgi/-tree-add-element e match element prepend)))
      (t (list e))))
   tree))

(defun corgi/-tree-contains (tree element)
  (seq-some
   (lambda (e)
     (cond
      ((equal e element) t)
      ((listp e) (corgi/-tree-contains e element))
      (t nil)))
   tree))

(defun corgi/enable-cider-connection-indicator-in-current-buffer ()
  (when (not (corgi/-tree-contains mode-line-format
                                   corgi/cider-connection-indicator-entry))
    (setq mode-line-format
          (corgi/-tree-add-element
           mode-line-format
           mode-line-buffer-identification
           corgi/cider-connection-indicator-entry
           t))))

(defun corgi/disable-cider-connection-indicator-in-current-buffer ()
  (setq mode-line-format
        (corgi/-tree-remove-element
         mode-line-format
         corgi/cider-connection-indicator-entry)))

(define-minor-mode corgi/cider-connection-indicator-mode
  "Display an indicator in the modeline showing which kind of CIDER
REPLs you are connceted to."
  :lighter ""
  (if corgi/cider-connection-indicator-mode
      (corgi/enable-cider-connection-indicator-in-current-buffer)
    (corgi/disable-cider-connection-indicator-in-current-buffer)))

(define-globalized-minor-mode
  corgi/global-cider-connection-indicator-mode
  corgi/cider-connection-indicator-mode
  (lambda ()
    (when (derived-mode-p 'clojure-mode)
      (corgi/cider-connection-indicator-mode 1))))

(provide 'corgi-cider-connection-indicator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-cider-connection-indicator.el ends here
