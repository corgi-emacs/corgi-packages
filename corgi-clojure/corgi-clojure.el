;;; corgi-clojure.el --- Clojure configuration for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-clojure.el
;; Package-Requires: ((use-package) (cider) (clj-ns-name) (clojure-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'use-package)

(use-package clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.ednl$" . clojure-mode))
  :config
  (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, we
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil))

(use-package cider
  :diminish cider-mode
  :config
  (setq cider-preferred-build-tool 'clojure-cli
        ;; ~make sure we can always debug nrepl issues~
        ;; Turning this off again, seems it may really blow up memory usage
        ;; nrepl-log-messages nil

        )

  ;; TODO: clean this up, submit to upstream where possible
  ;; More CIDER/clojure-mode stuff
  ;; - logical-sexp doesn't treat #_ correctly

  ;; New function, should go upstream. Kill all associated REPLs
  (defun corgi/cider-quit-all ()
    "Quit all current CIDER REPLs."
    (interactive)
    (let ((repls (seq-remove (lambda (r)
                               (equal r (get-buffer "*babashka-repl*")))
                             (seq-mapcat #'cdr (sesman-current-sessions 'CIDER)))))
      (seq-do #'cider--close-connection repls))
    ;; if there are no more sessions we can kill all ancillary buffers
    (cider-close-ancillary-buffers)
    ;; need this to refresh sesman browser
    (run-hooks 'sesman-post-command-hook))

  ;; When asking for a "matching" REPL (clj/cljs), and no matching REPL is found,
  ;; return any REPL that is there. This is so that cider-quit can be called
  ;; repeatedly to close all REPLs in a process. It also means that , s s will go
  ;; to any REPL if there is one open.
  (defun corgi/around-cider-current-repl (command &optional type ensure)
    (let ((repl (or
                 (if (not type)
                     (or (funcall command nil)
                         (funcall command 'any))
                   (funcall command type))
                 (get-buffer "*babashka-repl*"))))
      (if (and ensure (null repl))
          (cider--no-repls-user-error type)
        repl)))

  (advice-add #'cider-current-repl :around #'corgi/around-cider-current-repl)

  ;; This essentially redefines cider-repls. The main thing it does is return all
  ;; REPLs by using sesman-current-sessions (plural) instead of
  ;; sesman-current-session. It also falls back to the babashka repl if no repls
  ;; are connected/linked, so we can always eval.
  (defun corgi/around-cider-repls (_command &optional type ensure)
    (let ((type (cond
                 ((listp type)
                  (mapcar #'cider-maybe-intern type))
                 ((cider-maybe-intern type))))
          (repls (delete-dups (seq-mapcat #'cdr (or (sesman-current-sessions 'CIDER)
                                                    (when ensure
                                                      (user-error "No linked CIDER sessions"))))))
          (bb-repl (get-buffer "*babashka-repl*")))
      (or (seq-filter (lambda (b)
                        (and (cider--match-repl-type type b)
                             (not (equal b bb-repl))))
                      repls)
          (when bb-repl
            (list bb-repl)))))

  (advice-add #'cider-repls :around #'corgi/around-cider-repls)
  ;;  (advice-remove #'cider-repls #'corgi/around-cider-repls)

  (defvar-local corgi--clojure-indent-cache nil
    "Hashtable that caches indent information, for performance")

  (defun corgi/clojure-get-indent (function-name)
    (when (not corgi--clojure-indent-cache)
      (setq corgi--clojure-indent-cache (make-hash-table :test 'equal)))
    (let* ((no-props (substring-no-properties function-name))
           (indent (gethash no-props corgi--clojure-indent-cache nil)))
      (message "%S" no-props)
      (if indent
          indent
        (let ((indent (cider--get-symbol-indent function-name)))
          (puthash no-props indent corgi--clojure-indent-cache)
          indent))))

  ;; Leave off by default, still evaluating this
  ;; (setq cider-dynamic-indentation nil)
  ;; (setq clojure-get-indent-function 'corgi/clojure-get-indent)

  (defun corgi/cider-eval-last-sexp-and-replace ()
    "Alternative to cider-eval-last-sexp-and-replace, but kills
clojure logical sexp instead of ELisp sexp, and pprints the
result."
    (interactive)
    (let ((last-sexp (cider-last-sexp)))
      ;; we have to be sure the evaluation won't result in an error
      (cider-nrepl-sync-request:eval last-sexp)
      ;; seems like the sexp is valid, so we can safely kill it
      (let ((opoint (point)))
        (clojure-backward-logical-sexp)
        (kill-region (point) opoint))
      (cider-interactive-eval last-sexp
                              (cider-eval-pprint-with-multiline-comment-handler
                               (current-buffer)
                               (set-marker (make-marker) (point))
                               ""
                               " "
                               "")
                              nil
                              (cider--nrepl-print-request-map fill-column))))

  (defun corgi/cider-pprint-eval-last-sexp-insert ()
    (interactive)
    (let ((cider-comment-prefix "")
          (cider-comment-continued-prefix " ")
          (cider-comment-postfix ""))
      (cider-pprint-eval-last-sexp-to-comment)))

  (defadvice cider-find-var (before add-evil-jump activate)
    (evil-set-jump))

  (defun corgi/around-cider--choose-reusable-repl-buffer (_command _params)
    "Redefine cider--choose-reusable-repl-buffer to something more
sensible. If any dead REPL buffers exist when creating a new one
then simply delete them first. Return nil so `cider-create-repl'
creates a new one. Don't unnecessarily bother the user."
    (seq-do #'kill-buffer
            (seq-filter (lambda (b)
                          (with-current-buffer b
                            (and (derived-mode-p 'cider-repl-mode)
                                 (not (process-live-p (get-buffer-process b))))))
                        (buffer-list)))
    nil)

  (advice-add #'cider--choose-reusable-repl-buffer :around #'corgi/around-cider--choose-reusable-repl-buffer))

;; silence byte compiler
(require 'clojure-mode)
(require 'cider)

;; Most annoying JVM "feature" of all time
;; https://docs.cider.mx/cider/troubleshooting.html#empty-java-stacktraces
(defun corgi/around-cider-jack-in-global-options (command project-type)
  (if (eq 'clojure-cli project-type)
      (concat cider-clojure-cli-global-options
              " -J-XX:-OmitStackTraceInFastThrow")
    (funcall command project-type)))

(advice-add #'cider-jack-in-global-options :around #'corgi/around-cider-jack-in-global-options)
;; (use-package clj-refactor
;;   :after (cider)
;;   :diminish clj-refactor-mode
;;   :config
;;   (setq cljr-cljc-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
;;         cljr-cljs-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
;;         cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
;;         cljr-eagerly-build-asts-on-startup nil
;;         cljr-warn-on-eval nil)
;;   :hook ((clojurex-mode-hook
;;           clojurescript-mode-hook
;;           clojurec-mode-hook
;;           clojure-mode-hook)
;;          . clj-refactor-mode))

(use-package clj-ns-name
  :config
  (clj-ns-name-install))

(provide 'corgi-clojure)

(defun corgi/cider-pprint-eval-register (register)
  "Evaluate a Clojure snippet stored in a register.

Will ask for the register when used interactively. Put `#_clj' or
`#_cljs' at the start of the snippet to force evaluation to go to
a specific REPL type, no matter the mode (clojure-mode or
clojurescript-mode) of the current buffer.

You can use {{...}} to insert emacs-lisp code that willg get
evaluated, like `(println \"{{buffer-file-name}}\")'.
"
  (interactive (list (register-read-with-preview "Eval register: ")))
  (let ((reg (replace-regexp-in-string
              "{{\\([^}]+\\)}}"
              (lambda (s)
                (eval
                 (read
                  (match-string 1 s))))
              (get-register register))))
    (cond
     ((string-match-p "^#_cljs" reg)
      (with-current-buffer (car (cider-repls 'cljs))
        (cider--pprint-eval-form reg)))
     ((string-match-p "^#_clj" reg)
      (with-current-buffer (car (cider-repls 'clj))
        (cider--pprint-eval-form reg)))
     (t
      (cider--pprint-eval-form reg)))))

;; Backwards compatibility
(defalias 'corgi/cider-pprint-register #'corgi/cider-pprint-eval-register)

(defun corgi/cider-jack-in-babashka (&optional project-dir)
  "Start a utility CIDER REPL backed by Babashka, not related to a
specific project."
  (interactive)
  (when (get-buffer "*babashka-repl*")
    (kill-buffer "*babashka-repl*"))
  (let ((project-dir (or project-dir (projectile-project-root) user-emacs-directory)))
    (nrepl-start-server-process
     project-dir
     "bb --nrepl-server 0"
     (lambda (server-buf)
       (set-process-query-on-exit-flag
        (get-buffer-process server-buf) nil)
       (cider-nrepl-connect
        (list :repl-buffer server-buf
              :repl-type 'clj
              :host (plist-get nrepl-endpoint :host)
              :port (plist-get nrepl-endpoint :port)
              :project-dir project-dir
              :session-name "babashka"
              :repl-init-function (lambda ()
                                    (setq-local cljr-suppress-no-project-warning t
                                                cljr-suppress-middleware-warnings t
                                                process-query-on-exit-flag nil)
                                    (set-process-query-on-exit-flag
                                     (get-buffer-process (current-buffer)) nil)
                                    (rename-buffer "*babashka-repl*"))))))))

(defun corgi/cider-modeline-info ()
  (when (or (derived-mode-p 'clojure-mode)
            (derived-mode-p 'org-mode))
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



(defun corgi/enable-cider-connection-indicator-in-current-buffer ()
  (when (not (seq-find (lambda (e) (eq e '(:eval (corgi/cider-modeline-info)))) mode-line-format))
    (setq corgi/original-mode-line-format mode-line-format)
    (setq mode-line-format
          (seq-mapcat
           (lambda (e)
             (if (eq 'mode-line-modes e)
                 '(" " (:eval (corgi/cider-modeline-info)) " " mode-line-modes)
               (list e)))
           mode-line-format))))

(defun corgi/disable-cider-connection-indicator-in-current-buffer ()
  (when corgi/original-mode-line-format
    (setq mode-line-format corgi/original-mode-line-format)))

(defun corgi/enable-cider-connection-indicator ()
  "In Clojure buffers show an indicator in the modeline for which
CIDER REPLs the current buffer is linked to, with color coding
for clj/cljs/bb, and extra info if the link goes to a different
project or host."
  (interactive)
  (add-hook 'clojure-mode-hook #'corgi/enable-cider-connection-indicator-in-current-buffer)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when  (or (derived-mode-p 'clojure-mode)
                 (derived-mode-p 'org-mode))
        (corgi/enable-cider-connection-indicator-in-current-buffer)))))

(defun corgi/disable-cider-connection-indicator ()
  "In Clojure buffers show an indicator in the modeline for which
CIDER REPLs the current buffer is linked to, with color coding
for clj/cljs/bb, and extra info if the link goes to a different
project or host."
  (interactive)
  (remove-hook 'clojure-mode-hook #'corgi/enable-cider-connection-indicator-in-current-buffer)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when corgi/original-mode-line-format
        (corgi/disable-cider-connection-indicator-in-current-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-clojure.el ends here
