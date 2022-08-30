;;; init-lang-php.el --- config for php -*- lexical-binding: t -*-
;;; Commentary:
;; Thanks to Eglot, we just need to install the `php-mode'.
;;; Code:

(defvar php-executable "/usr/local/opt/php@7.4/bin/php")
;; Enable auto-complete-mode
(use-package auto-complete
 :defer 1)
(auto-complete-mode t)
(use-package ac-php)

(use-package php-mode
  :hook (php-mode
          . (lambda ()
             (setq ac-sources '(ac-source-php))
             ;; As an example (optional)
             (yas-global-mode 1)
             ;; Enable ElDoc support (optional)
             (ac-php-core-eldoc-setup)
             )))
(defun bs-php-mode-hook ()
  (setq php-template-compatibility nil))

(add-hook 'php-mode-hook 'bs-php-mode-hook)

(provide 'init-lang-php)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lang-php.el ends here
