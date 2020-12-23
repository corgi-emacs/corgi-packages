(message "INIT LAMBDAISLAND STRAIGHT")

(defun straight-recipes-corgi-packages-list ()
  (list "corgi" "corkey"))

(defun straight-recipes-corgi-packages-retrieve (package)
  (pcase package
    (`corgi
     '(corgi :type git
             :repo "/home/arne/github/lambdaisland/corgi"))
    (`corkey
     '(corkey
       :type git
       :host github
       :branch "main"
       :files ("corkey/corkey.el")
       :repo "lambdaisland/corgi-packages"))
    (_ nil)))

(add-to-list #'straight-recipe-repositories 'corgi-packages)

(provide 'corgi-packages)
