;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;; Github: https://github.com/yaoyue126/emacs.d

;;; Code:

;; set the startup default directory, not essential but recommended.
(setq default-directory "~/")

;; update load-path to make customized lisp codes work
(dolist (folder (directory-files (concat user-emacs-directory "lisp") t directory-files-no-dot-files-regexp))
  (add-to-list 'load-path folder))

;; change Emacs default settings here, variables only (NOT include built-in packages)
(require 'init-system)

;; settings for Melpa/Elpa/GNU repos for Emacs package manager
(require 'init-elpa)

;; all the third-part packages configed here
(require 'init-package)

;; settings for programming languages (include IDE/LSP feature)
(require 'init-lang)

;; windows number
;;(require 'init-winum)

;; calendar
(require 'init-calendar)

;; email mu4e
(require 'init-mu4e)

;; custom setting
(require 'init-setting)

;; DON'T forget to define and load custom file at last
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
