;;; corgi-packages.el --- Straight package repo and helpers for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-packages.el
;; Package-Requires: ((straight))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'straight)

(defvar corgi-all-packages
  (append '((clj-ns-name
             :type git
             :host github
             :files ("clj-ns-name.el")
             :repo "corgi-emacs/clj-ns-name")

            (walkclj
             :type git
             :host github
             :files ("walkclj.el")
             :repo "corgi-emacs/walkclj")

            (pprint-to-buffer
             :type git
             :host github
             :files ("pprint-to-buffer/pprint-to-buffer.el")
             :repo "plexus/plexmacs")

            (corkey
             :type git
             :host github
             :branch "main"
             :files ("corkey.el")
             :repo "corgi-emacs/corkey")

            (corgi-bindings
             :type git
             :host github
             :branch "main"
             :files ("corgi-bindings/corgi-bindings.el"
                     "corgi-bindings/corgi-keys.el"
                     "corgi-bindings/corgi-signals.el"
                     "corgi-bindings/user-keys-template.el"
                     "corgi-bindings/user-signals-template.el")
             :repo "corgi-emacs/corgi-packages")

            (piglet-emacs
             :type git
             :host github
             :files ("piglet-emacs.el" "piglet-emacs-pkg.el"
                     "piglet-mode.el" "pdp.el")
             :repo "piglet-lang/piglet-emacs"))

          (mapcar
           (lambda (pkg)
             `(,pkg
               :type git
               :host github
               :branch "main"
               :files (,(concat (symbol-name pkg) "/" (symbol-name pkg) ".el"))
               :repo "corgi-emacs/corgi-packages"))

           '(corgi-defaults
             corgi-editor
             corgi-commands
             corgi-emacs-lisp
             corgi-emacs
             corgi-clojure
             corgi-stateline))))

(defun straight-recipes-corgi-packages-list ()
  (mapcar #'symbol-name (mapcar #'car corgi-all-packages)))

(defun straight-recipes-corgi-packages-retrieve (package)
  (assoc package corgi-all-packages))

(defun straight-recipes-corgi-packages-version ()
  1)

(add-to-list 'straight-recipe-repositories 'corgi-packages)

;; Check if straight/versions/corgi.el exists in the user's emacs directory. If not, then
;; we copy it over from Corgi.

(defun corgi-version-file-path ()
  (expand-file-name "straight/versions/corgi.el" straight-base-dir))

(defun corgi-copy-versions-file ()
  (interactive)
  (let* ((version-file-source (expand-file-name "straight/repos/corgi-packages/corgi-versions.el" straight-base-dir))
         (straight-version-dir (expand-file-name "straight/versions" straight-base-dir))
         (version-file-target (corgi-version-file-path)))
    (unless (file-exists-p straight-version-dir)
      (make-directory straight-version-dir t))
    (copy-file version-file-source version-file-target t)))

(defun corgi/upgrade-self ()
  "Upgrade Corgi

   Fetch the latest corgi-packages, and make straight use the
versions specified therein. Will overwrite any local changes to
straight/versions/corgi.el"
  (interactive)
  (straight-pull-package "corgi-packages")
  (corgi-copy-versions-file)
  (straight-thaw-versions))

(defalias 'corgi-upgrade-self 'corgi/upgrade-self)

(when (not (file-exists-p (corgi-version-file-path)))
  (corgi-copy-versions-file))

(add-to-list 'straight-profiles '(corgi . "corgi.el"))

;; It sucks that I'm putting this here, but depending on which package gets to
;; pull in evil first we keep getting warnings from evil-collection. Seems to
;; mostly happen after straight clones/builds new or updated packages. So I'm
;; defensively setting it here, this is the earliest chance we get apart from
;; asking users to put it directly in their config.
(with-no-warnings (setq evil-want-keybinding nil))

(provide 'corgi-packages)
