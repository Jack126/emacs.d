;;; init-common.el --- customized functions -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-
;;; Code:

;; customized functions
(require 'init-fn)

;; change Emacs default settings here, variables only (NOT include built-in packages)
(require 'init-system)

;; settings for Melpa/Elpa/GNU repos for Emacs package manager
(require 'init-elpa)

;; windows number
(require 'init-winum)

;; calendar
(require 'init-calendar)

;; all the third-part packages configed here
(require 'init-package)

;; change default Emacs settings with built-in packages
(require 'init-builtin)

;; custom setting
(require 'init-setting)

;; email mu4e
(require 'init-mu4e)


(provide 'init-common)
;;; init-common.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
