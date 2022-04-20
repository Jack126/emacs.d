;;; init-lang-php.el --- config for php -*- lexical-binding: t -*-
;;; Commentary:
;; Thanks to Eglot, we just need to install the `php-mode'.
;;; Code:

;; php
(use-package phpactor :ensure t)
(use-package company-phpactor :ensure t)
(defvar phpactor-executable "/Users/Jack/workspace/emacs/cabins-emacs.d/phpactor/bin/phpactor")

(use-package php-mode
  :hook ((php-mode . (lambda () (set (make-local-variable 'company-backends)
       '(;; list of backends
         company-phpactor
         company-files
         ))))))
(add-hook 'php-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function
                  'phpactor-hover)))
        ;; (add-hook 'phpactor-after-update-file-hook
        ;;     (lambda () (save-buffer))))

(provide 'init-lang-php)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lang-php.el ends here
