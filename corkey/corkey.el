;;; corkey.el --- Keybinding mechanics for Corgi
;;
;; Copyright (C) 2020 Gaiwan GmbH
;;
;; Author: Arne Brasseur <arne@gaiwan.co>
;; Package-Requires: ((use-package) (a) (evil) (which-key))
;;
;;; Code:

(require 'use-package)

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

(defun corkey/setup-keymaps (bindings signals)
  (mapc
   (lambda (binding)
     (pcase-let ((`(,mode ,keys ,desc ,target) binding))
       (which-key-add-key-based-replacements keys desc)
       (cond
        ;; Prefixes
        ((not target))
        ;; Signal dispatch
        ((keywordp target)
         (let ((mode-targets (cdr (assoc target signals))))
           (mapc
            (lambda (mode-target)
              (let* ((mode-name (car mode-target))
                     (mode-var (if (boundp mode-name)
                                   mode-name
                                 (intern (concat "corkey--" (symbol-name mode-name)))))
                     (rest (cdr mode-target)))
                (if (symbolp rest)
                    (evil-define-minor-mode-key mode mode-var (kbd keys) rest)
                  ;; Major-mode specific description
                  (evil-define-minor-mode-key mode mode-var (kbd keys) (cadr rest))
                  (which-key-add-major-mode-key-based-replacements mode-name keys (car rest)))))
            mode-targets)))
        ;; Direct mapping to command
        ((symbolp target)
         (evil-define-minor-mode-key mode 'corkey-local-mode (kbd keys) target)))))
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
