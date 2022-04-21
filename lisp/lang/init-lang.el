;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins

;;; Commentary:
;;; Code:

;; Common features when programming
(add-hook 'prog-mode-hook
          (lambda ()
            (electric-layout-mode)
            (electric-pair-mode)
            (hs-minor-mode)
            (display-line-numbers-mode)))

;; cc-mode
(add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state)


;; Language Server
(require 'init-eglot) ; eglot
;; (require 'init-lsp)   ; lsp, enable this line if you like lsp-mode and disable eglot line

;; Specific Languages
(require 'init-lang-go)
(require 'init-lang-python)
(require 'init-lang-rust)
(require 'init-lang-web)
(require 'init-lang-php)

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
