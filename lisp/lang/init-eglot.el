;;; init-eglot.el --- config for eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :hook ((c-mode
          c++-mode
          go-mode
          java-mode
          js-mode
          python-mode
          lua-mode
          rust-mode
          web-mode) . eglot-ensure)
  :bind (("C-c e f" . #'eglot-format)
         ("C-c e a" . #'eglot-code-actions)
         ("C-c e i" . #'eglot-code-action-organize-imports)
         ("C-c e q" . #'eglot-code-action-quickfix))
  :config
  (progn
    (setq eldoc-echo-area-use-multiline-p 3
          eldoc-echo-area-display-truncation-message nil)
    (set-face-attribute 'eglot-highlight-symbol-face nil
                        :background "#b3d7ff"))
  (add-to-list 'eglot-server-programs '(web-mode "vls"))
  )
(provide 'init-eglot)

;;; init-eglot.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
