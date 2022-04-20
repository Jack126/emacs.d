;;; init-eglot.el --- config for eglot -*- lexical-binding: t -*-
;;; Commentary:
;;
;; If you don't like eglot/lsp-mode for specific languages, some alternatives are:
;; - `java-mode' with `meghanada-mode' & `meghanada-server'
;; - `python-mode' with `elpy'

;;; Code:
;; (defvar phpactor-executable "/usr/local/bin/phpactor")

;; (defclass eglot-php (eglot-lsp-server) ()
;;     :documentation "Phpactor lsp server.")

(use-package eglot
  :hook ((c-mode
          c++-mode
          go-mode
          java-mode
          js-mode
          python-mode
          rust-mode
          php-mode
          web-mode) . eglot-ensure)
  :bind (("C-c e f" . #'eglot-format)
         ("C-c e a" . #'eglot-code-actions)
         ("C-c e i" . #'eglot-code-action-organize-imports)
         ("C-c e q" . #'eglot-code-action-quickfix))
  :config
  ;; (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (add-to-list 'eglot-server-programs 
        '(web-mode "vls")
        ;;`(php-mode . (eglot-php . ("php" "-d" "memory_limit=1024M" ,phpactor-executable "language-server")))
        )
;;   (cl-defmethod eglot-initialization-options ((server eglot-php))
;;     "Passes through required vetur initialization options to VLS."
;;     `(:language_server.session_parameters []
;;                                           :indexer.exclude_patterns []
;;                                           :language_server_code_transform.import_globals ,t))
  (defun eglot-actions-before-save()
    (add-hook 'before-save-hook
              (lambda ()
                (call-interactively #'eglot-format)
                (call-interactively #'eglot-code-action-organize-imports))))
  (add-hook 'eglot--managed-mode-hook #'eglot-actions-before-save))

(provide 'init-eglot)

;;; init-eglot.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
