;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins

;;; Commentary:
;;; Code:

;; language server
(require 'init-eglot) ; eglot
;; (require 'init-lsp)   ; lsp, enable this line if you like lsp-mode and disable eglot line

;; specific languages
(require 'init-lang-go)
(require 'init-lang-python)
(require 'init-lang-rust)
(require 'init-lang-web)
(require 'init-lang-php)

;; program useful text/config files
(use-package json-mode)
(use-package markdown-mode)
(use-package protobuf-mode)
(use-package yaml-mode)

;; useful tools
(use-package quickrun)                  ; quickrun code
(use-package restclient                 ; restclient support
  :mode (("\\.http\\'" . restclient-mode)))

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
