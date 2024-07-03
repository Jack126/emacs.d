;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;;; Commentary:
;;; Code: lsp-bridge config

;; Common features when programming
;; **************************************************

(unless (display-graphic-p)
  (add-to-list 'load-path "~/.emacs.d/modules/popon")
  (add-to-list 'load-path "~/.emacs.d/modules/acm-terminal"))

(unless (display-graphic-p)
  (with-eval-after-load 'acm
    (require 'acm-terminal)))


(add-to-list 'load-path "~/.emacs.d/modules/lsp-bridge")

(require 'lsp-bridge)

(setq lsp-bridge-enable-completion-in-minibuffer t)
(setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
(setq lsp-bridge-enable-with-tramp t)
(setq lsp-bridge-enable-org-babel t)
(setq acm-enable-quick-access t)
(setq acm-backend-yas-match-by-trigger-keyword t)
(setq acm-enable-tabnine nil)
(setq acm-enable-codeium nil)
(setq lsp-bridge-enable-log nil)

(global-lsp-bridge-mode)

(provide 'init-lsp)

;;; init-lsp.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
