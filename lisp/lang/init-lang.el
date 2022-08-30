;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins

;;; Commentary:
;;; Code:

;; Common features when programming
;; **************************************************
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode)
            (electric-layout-mode)
            (electric-pair-mode)
            (hs-minor-mode)
            (prettify-symbols-mode)))

;; cc-mode
(add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state)

;; Flymake
(add-hook 'prog-mode-hook 'flymake-mode)
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

;; Language Server
;; **************************************************
;; `eglot', a light-weight LSP client
(require 'init-eglot)
;; `lsp-mode', a full-feature LSP client
;; (require 'init-lsp)

;; Golang
(use-package go-mode)
;; lua
(use-package lua-mode)

;;rust
(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

;;web
;; use C-j to expand emmet
(use-package emmet-mode
  :hook ((web-mode css-mode) . emmet-mode))

(use-package web-mode
  :init
  ;; use web-mode to handle vue/html files
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t))

(use-package typescript-mode)

(defun bs-web-mode-hook ()
  (local-set-key '[backtab] 'indent-relative)
  (setq indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'bs-web-mode-hook)


;; Program Useful text/config files
(use-package json-mode)
(use-package markdown-mode)
(use-package protobuf-mode)

;; Useful Tools
(use-package quickrun)                  ; quickrun code
(use-package restclient                 ; restclient support
  :mode (("\\.http\\'" . restclient-mode)))

;; Specific Languages
(require 'init-lang-python)
(require 'init-lang-php)

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
