;;; init-lang-php.el --- config for php -*- lexical-binding: t -*-
;;; Commentary:
;; Thanks to Eglot, we just need to install the `php-mode'.
;;; Code:

;; php
;; A method , use-package
(defvar php-executable "/usr/local/opt/php@7.4/bin/php")

(use-package php-mode;;
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
(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "M-.") #'phpactor-goto-definition)
  (define-key php-mode-map (kbd "M-?") #'phpactor-find-references))
;; B method , git clone https://github.com/emacs-php/php-mode

;; (when (file-directory-p "~/.emacs.d/modules/php-mode/lisp/")
;;   (load "~/.emacs.d/modules/php-mode/lisp/php-mode-autoloads.el"))



;; phpactor
(use-package phpactor :ensure t)
(use-package company-phpactor :ensure t)
(provide 'init-lang-php)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lang-php.el ends here
