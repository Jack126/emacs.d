;;; init-lang-php.el --- config for php -*- lexical-binding: t -*-
;;; Commentary:
;; Thanks to Eglot, we just need to install the `php-mode'.
;;; Code:

;; php
;; A method , use-package
(defvar php-executable "/usr/local/opt/php@7.4/bin/php")

;; Enable auto-complete-mode
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
  (setq indent-tabs-mode nil)
  (setq php-template-compatibility nil)
  (setq c-basic-offset 2))

(add-hook 'php-mode-hook 'bs-php-mode-hook)
(provide 'init-lang-php)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lang-php.el ends here
