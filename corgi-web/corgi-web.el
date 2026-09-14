;;; corgi-web.el --- Web related modes and config -*- lexical-binding: t -*-
;;
;; Filename: corgi-web.el
;; Package-Requires: ((use-package) (web-mode) (markdown-mode) (rainbow-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'use-package)

;; Plain HTML stays with the built-in `mhtml-mode', which delegates embedded
;; <script>/<style> content to `js-mode'/`css-mode'.  `web-mode' is only used
;; for template files, where there is no built-in equivalent.
(use-package web-mode
  :mode ("\\.erb\\'"
         "\\.twig\\'"
         "\\.jinja2?\\'"))

;; TypeScript/TSX and YAML use the built-in tree-sitter modes (Emacs 29+). The
;; first time you open such a file, Emacs offers to install the grammar;
;; alternatively run `M-x treesit-install-language-grammar'.
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;; On newer emacs, prefer markdown-ts-mode, otherwise, install markdown-mode
(if (fboundp #'markdown-ts-mode)
    (progn
      (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-ts-mode)))
  (use-package markdown-mode))

;; Colorize color literals (hex codes, rgb(), ...) in web buffers.
(use-package rainbow-mode)

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))

(defvar corgi-web-default-offset 2
  "Default indentation width for web-related modes.")

(setq-default js-indent-level corgi-web-default-offset
              web-mode-markup-indent-offset corgi-web-default-offset
              web-mode-css-indent-offset corgi-web-default-offset
              web-mode-code-indent-offset corgi-web-default-offset
              web-mode-attr-indent-offset corgi-web-default-offset)

(provide 'corgi-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-web.el ends here
