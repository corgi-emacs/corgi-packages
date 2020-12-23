(defvar corgi-all-packages
  '((corgi
     :type git
     :repo "/home/arne/github/lambdaisland/corgi")

    (corkey
     :type git
     :host github
     :branch "main"
     :files ("corkey/corkey.el")
     :repo "lambdaisland/corgi-packages")

    (clj-ns-name
     :type git
     :host github
     :files ("clj-ns-name/clj-ns-name.el")
     :repo "plexus/plexmacs")

    (walkclj
     :type git
     :host github
     :files ("walkclj.el")
     :repo "plexus/walkclj")))

(defun straight-recipes-corgi-packages-list ()
  (mapcar #'symbol-name (mapcar #'car corgi-all-packages)))

(defun straight-recipes-corgi-packages-retrieve (package)
  (assoc package corgi-all-packages))

(defun straight-recipes-corgi-packages-version ()
  1)

(add-to-list #'straight-recipe-repositories 'corgi-packages)

(provide 'corgi-packages)
