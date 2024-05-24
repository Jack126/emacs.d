;;; init-eglot.el --- config for eglot -*- lexical-binding: t -*-
;;; Commentary:
;;
;; If you don't like eglot/lsp-mode for specific languages, some alternatives are:
;; - `java-mode' with `meghanada-mode' & `meghanada-server'
;; - `python-mode' with `elpy'

;;; Code:

(use-package eglot
  :hook ((c-mode
          c++-mode
          go-mode
          java-mode
          js-mode
          python-mode
          lua-mode
          php-mode
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
  ;; (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (add-to-list 'eglot-server-programs '((web-mode "vls") (php-mode "phpactor" "language-server")))
    ;; (defun eglot-actions-before-save()
    ;;   (add-hook 'before-save-hook #'eglot-format-buffer))
    ;; (add-hook 'eglot--managed-mode-hook #'eglot-actions-before-save)
  )

(provide 'init-eglot)

;;; init-eglot.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
