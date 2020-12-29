;;; corkey.el --- Keybinding mechanics for Corgi
;;
;; Copyright (C) 2020 Gaiwan GmbH
;;
;; Author: Arne Brasseur <arne@gaiwan.co>
;; Package-Requires: ((use-package) (a) (evil) (which-key))
;;
;;; Code:

(use-package evil :init (setq evil-want-keybinding nil))
(use-package which-key)
(use-package a)

(define-minor-mode corkey-local-mode
  "Minor mode providing corkey bindings"
  :lighter ""
  ;; To have bindings that are specific to a major mode, without actually
  ;; changing that major-mode's mode-map, we fake a minor mode (really just a
  ;; variable) that is true/on when the given major-mode is enabled (it shadows
  ;; the major mode, hence the name). When loading key bindings into evil we
  ;; associate them with this shadow minor mode. This way the corkey bindings
  ;; remain isolated and can easily be toggled.
  (let ((shadow-mode-var (intern (concat "corkey--" (symbol-name major-mode)))))
    (if corkey-local-mode
        (progn
          (make-variable-buffer-local shadow-mode-var)
          (set shadow-mode-var t))
      (set shadow-mode-var nil))))

(defun corkey-initialize ()
  (unless (and (minibufferp) (not evil-want-minibuffer))
    (corkey-local-mode)))

(define-globalized-minor-mode corkey-mode
  corkey-local-mode
  corkey-initialize)

(defun corkey/-flatten-bindings (mode prefix bindings)
  (let ((head (car bindings))
        (rest (cdr-safe bindings)))
    (if (symbolp head)
        (seq-mapcat (lambda (b)
                      (corkey/-flatten-bindings head prefix b))
                    rest)
      (let ((desc (car-safe rest))
            (rest (cdr-safe rest)))
        (if (consp (car rest))
            (cl-list* (list mode (concat prefix head) desc)
                      (seq-mapcat (lambda (b)
                                    (corkey/-flatten-bindings mode (concat prefix head " ") b))
                                  rest))
          (list (list mode (concat prefix head) desc (car rest))))))))

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

(defun corkey/setup-keymaps (bindings signals)
  (mapc
   (lambda (binding)
     (pcase-let ((`(,mode ,keys ,desc ,target) binding))
       (cond
        ((not target))
        ((keywordp target)
         (let ((mode-targets (cdr (assoc target signals))))
           (mapc
            (lambda (mode-target)
              (let ((major-mode (car mode-target))
                    (shadow-mode-var (intern (concat "corkey--" (symbol-name major-mode))))
                    (rest (cdr mode-target)))
                (if (symbolp rest)
                    (evil-define-minor-mode-key mode shadow-mode-var (kbd keys) rest)
                  (evil-define-minor-mode-key mode shadow-mode-var (kbd keys) (cadr rest))
                  (which-key-add-key-based-replacements keys (car rest)))))
            mode-targets)))
        ((symbolp target)
         (evil-define-minor-mode-key mode 'corkey-local-mode (kbd keys) target)
         (which-key-add-key-based-replacements keys desc)))))
   bindings)
  nil)

(defun corkey/-read-file (file-name)
  (with-current-buffer (find-file-noselect file-name)
    (auto-revert-mode 1)
    (goto-char (point-min))
    (read (current-buffer))))

(defun corkey/-locate-file (fname)
  (cond
   ((symbolp fname)
    (corkey/-locate-file (concat (symbol-name fname) ".el")))
   ((file-exists-p (expand-file-name fname user-emacs-directory))
    (expand-file-name fname user-emacs-directory))
   (t (locate-library fname))))

(defun corkey/install-bindings (&optional binding-files signal-files)
  (interactive)
  (let* ((binding-files (or binding-files 'corgi-keys))
         (signal-files (or signal-files 'corgi-signals))

         (binding-files (if (listp binding-files)
                            binding-files
                          (list binding-files)))
         (signal-files (if (listp signal-files)
                           signal-files
                         (list signal-files)))
         (bindings (seq-mapcat
                    (lambda (f)
                      (thread-last f
                        corkey/-locate-file
                        corkey/-read-file
                        (corkey/-flatten-bindings 'normal "")))
                    binding-files))
         (signals (seq-reduce
                   #'corkey/-flatten-signals
                   (mapcar (lambda (f)
                             (corkey/-read-file (corkey/-locate-file f)))
                           signal-files)
                   nil)))

    (corkey/setup-keymaps bindings signals)))

(provide 'corkey)

;;; corkey.el ends here
