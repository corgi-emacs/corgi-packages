;;; corkey.el --- Keybinding mechanics for Corgi
;;
;; Copyright (C) 2020 Gaiwan GmbH
;;
;; Author: Arne Brasseur <arne@gaiwan.co>
;; Package-Requires: ((evil "1.14.0") (which-key "3.5.0"))
;;
;;; Code:

(use-package evil :init (setq evil-want-keybinding nil))
(use-package which-key)

(defvar corkey/keymap (make-sparse-keymap))

(defcustom corkey/key-binding-files
  nil
  "Files with keybinding definitions read by corkey. Later
  entries override earlier entries")

(defun corkey/-read-file (file-name)
  (with-current-buffer (find-file-noselect file-name)
    (auto-revert-mode 1)
    (goto-char (point-min))
    (read (current-buffer))))

(defun corkey/-kv-list-get (list key)
  (while (and list (not (eql key (car list))))
    (setq list (cddr list)))
  (cadr list))

(defun corkey/-flatten-bindings (prefix bindings)
  (let ((p (car bindings))
        (desc (cadr bindings))
        (rest (cddr bindings)))
    (if (symbolp (car rest))
        (list (list (concat prefix p) desc (car rest)))
      (cl-list* (list (concat prefix p) desc)
                (seq-mapcat (lambda (b)
                              (corkey/-flatten-bindings (concat prefix p " ") b))
                            rest)))))

(defun corkey/-set-bindings* (keymap bindings command-mapping)
  (setcdr keymap nil)
  (cl-loop
   for binding-set in bindings
   do
   (cl-loop
    for (keys desc cmd) in (corkey/-flatten-bindings "" binding-set)
    do
    (cond
     ((keywordp cmd)
      (let ((sym (corkey/-kv-list-get command-mapping cmd)))
        (when sym
          (when (listp sym)
            (setq desc (car sym))
            (setq sym (cadr sym)))
          (progn
            ;; We stick these in the map for the current major mode, since these
            ;; are major mode specific, so having them in our shared minor mode
            ;; map causes issues. The downside here is it makes it harder to
            ;; clean things up when the minor mode gets disabled. Not a huge
            ;; issue for me right now but eventually we'll need a better
            ;; approach, possibly adding to minor-mode-map-alist with synthetic
            ;; "minor mode" variables each corresponding with a certain major
            ;; mode
            (evil-define-key 'normal (current-local-map) (kbd keys) sym)
            (evil-define-key 'motion (current-local-map) (kbd keys) sym)))))
     ((symbolp cmd)
      (progn
        (evil-define-key 'normal keymap (kbd keys) cmd)
        (evil-define-key 'motion keymap (kbd keys) cmd))))
    (which-key-add-key-based-replacements keys desc))))

(defun corkey/set-bindings (binding-spec)
  ;;(message "---> bindings for %s %s" major-mode (buffer-name))
  (let* ((bindings (corkey/-kv-list-get binding-spec :bindings))
         (modes (corkey/-kv-list-get binding-spec :modes))
         (commands
          (cl-loop
           for (mode mapping) in (reverse modes)
           append
           (when (or (eql 'global mode) (derived-mode-p mode))
             mapping))))
    (corkey/-set-bindings* corkey/keymap bindings commands))
  (evil-normalize-keymaps))

(defun corkey/maybe-set-bindings ()
  (interactive)
  (when corkey-mode
    (cl-loop
     for file in (reverse corkey/key-binding-files)
     do
     (corkey/set-bindings (corkey/-read-file (expand-file-name file user-emacs-directory))))))

(define-minor-mode corkey-mode
  "Multi leader mode"
  :keymap corkey/keymap
  ;; this needs fine tuning, but it already skips a lot of internal buffers like
  ;; *nrepl-decoding*
  (when (not (or  (eql major-mode 'fundamental-mode)
                  (eql major-mode 'minibuffer-inactive-mode)))
    ;;(message "corkey-mode (minor mode) %s" (buffer-name))
    (corkey/maybe-set-bindings)))

;;(corkey/set-bindings (corkey/-read-file (car corkey/key-binding-files)))

(define-global-minor-mode global-corkey-mode
  corkey-mode
  (lambda ()
    (when (not (minibufferp))
      (corkey-mode 1))))

(provide 'corkey)

;;; corkey.el ends here
