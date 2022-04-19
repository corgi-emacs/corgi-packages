;;; corgi-clojure-cider-extras.el --- CIDER extensions and patches -*- lexical-binding: t -*-
;;
;; Filename: corgi-clojure-cider-extras.el
;; Package-Requires: ((cider))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cider)

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

(defun corgi/nrepl-toggle-message-logging-local ()
  "Like `nrepl-toggle-message-logging', but only for the current connection"
  (interactive)
  (let* ((repls (cider-repl-buffers))
         (enabled? (with-current-buffer (car repls) nrepl-log-messages)))
    (mapcar (lambda (buf)
              (with-current-buffer buf
                (setq-local nrepl-log-messages (not enabled?))))
            repls)
    (message "nREPL message logging %s in %S"
             (if enabled? "disabled" "enabled")
             (mapcar #'buffer-name repls))))

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
(defun corgi/around-cider-repls (command &optional type ensure)
  (let ((type (cond
               ((listp type)
                (mapcar #'cider-maybe-intern type))
               ((cider-maybe-intern type))))
        (repls (delete-dups (seq-mapcat #'cdr (or (sesman-current-sessions 'CIDER)
                                                  (when ensure
                                                    (user-error "No linked %s sessions" system)))))))
    (or (seq-filter (lambda (b)
                      (and (cider--match-repl-type type b)
                           (not (equal b (get-buffer "*babashka-repl*")))))
                    repls)
        (list (get-buffer "*babashka-repl*")))))

(advice-add #'cider-repls :around #'corgi/around-cider-repls)

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

(defun corgi/around-cider--choose-reusable-repl-buffer (command params)
  "Redefine cider--choose-reusable-repl-buffer to something more
sensible. If any dead REPL buffers exist when creating a new one
then simply delete them first. Return nil co `cider-creat-repl'
creates a new one. Don't unnecessarily bother the user."
  (seq-do #'kill-buffer
          (seq-filter (lambda (b)
                        (with-current-buffer b
                          (and (derived-mode-p 'cider-repl-mode)
                               (not (process-live-p (get-buffer-process b))))))
                      (buffer-list)))
  nil)

(advice-add #'cider--choose-reusable-repl-buffer :around #'corgi/around-cider--choose-reusable-repl-buffer)

;; Most annoying JVM "feature" of all time
;; https://docs.cider.mx/cider/troubleshooting.html#empty-java-stacktraces
(defun corgi/around-cider-jack-in-global-options (command project-type)
  (if (eq 'clojure-cli project-type)
      (concat cider-clojure-cli-global-options
              (when cider-clojure-cli-global-options " ")
              "-J-XX:-OmitStackTraceInFastThrow")
    (command project-type)))

(advice-add #'cider-jack-in-global-options :around #'corgi/around-cider-jack-in-global-options)

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
  (let ((project-dir (or project-dir user-emacs-directory)))
    (nrepl-start-server-process
     project-dir
     "bb --nrepl-server 0"
     (lambda (server-buffer)
       (set-process-query-on-exit-flag
        (get-buffer-process server-buffer) nil)
       (cider-nrepl-connect
        (list :repl-buffer server-buffer
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

(provide 'corgi-clojure-cider-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-clojure-cider-extras.el ends here
