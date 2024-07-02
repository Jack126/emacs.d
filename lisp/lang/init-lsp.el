
;; (use-package lsp-bridge
;;   :ensure t
;;   :load-path "~/.emacs.d/modules/lsp-bridge")
(unless (display-graphic-p)
  (add-to-list 'load-path "~/workspace/emacs/cabins-emacs/modules/popon")
  (add-to-list 'load-path "~/workspace/emacs/cabins-emacs/modules/acm-terminal"))

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