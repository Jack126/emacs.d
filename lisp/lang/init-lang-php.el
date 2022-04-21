;;; init-lang-php.el --- config for php -*- lexical-binding: t -*-
;;; Commentary:
;; Thanks to Eglot, we just need to install the `php-mode'.
;;; Code:

;; php
;; A method , use-package
(use-package php-mode)
(defvar php-executable "/usr/local/opt/php@7.4/bin/php")
;; B method , git clone https://github.com/emacs-php/php-mode

;; (when (file-directory-p "~/.emacs.d/modules/php-mode/lisp/")
;;   (load "~/.emacs.d/modules/php-mode/lisp/php-mode-autoloads.el"))


(provide 'init-lang-php)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lang-php.el ends here
