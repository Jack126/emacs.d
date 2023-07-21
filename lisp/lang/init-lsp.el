;;; init-lsp.el --- settings for lsp mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Minimized lsp-mode setup
(add-to-list 'load-path "~/.emacs.d/modules/lsp-bridge")

;; (require 'yasnippet)
;;(yas-global-mode 1)

(require 'lsp-bridge)
(global-lsp-bridge-mode)
(setq lsp-bridge-python-lsp-server "pyright")
(provide 'init-lsp)
;;; init-lsp.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
