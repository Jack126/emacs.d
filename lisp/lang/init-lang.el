;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; Common features when programming
;; **************************************************
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode)
            (electric-layout-mode)
            (electric-pair-mode)
            (hs-minor-mode)
            (prettify-symbols-mode)))

;; cc-mode
(add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state)

;; Flymake
(add-hook 'prog-mode-hook 'flymake-mode)
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

;; Language Server
;; **************************************************
;; `eglot', a light-weight LSP client
(require 'init-eglot)
;; `lsp-mode', a full-feature LSP client
;; (require 'init-lsp)

;; Golang
(use-package go-mode)
;; lua
(use-package lua-mode)

;;rust
(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

;;web
;; use C-j to expand emmet
(use-package emmet-mode
  :hook ((web-mode css-mode) . emmet-mode))

(use-package web-mode
  :init
  ;; use web-mode to handle vue/html files
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t))

(use-package typescript-mode)

(defun bs-web-mode-hook ()
  (local-set-key '[backtab] 'indent-relative)
  (setq indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'bs-web-mode-hook)


;;python
(defun check-run-command (command arg-string)
  "Find the EXEC-FILE and run the BODY."

  (if (not (executable-find command))
      (message "[ERROR]: <%s> not found!" command)
    (save-buffer)
    (shell-command (format "%s %s" command arg-string))
    (revert-buffer t t t)))

;; BE CAREFUL! Maybe bugs here, always call this function manually.
;;;###autoload
(defun python-isort ()
  "Sort the imports with isort."
  (interactive)
  (check-run-command "isort"
		     (format "--atomic --profile=black %s"
			     (buffer-file-name))))

;; BE CAREFUL! Maybe bugs here, always call this function manually.
;;;###autoload
(defun python-remove-all-unused-imports ()
  "Remove all the unused imports, do NOT use pyimport, as it has bugs.
eg.from datetime import datetime."
  (interactive)
  (check-run-command "autoflake"
		     (format "-i --remove-all-unused-imports %s"
			     (buffer-file-name))))

(add-hook
 'python-mode-hook
 (lambda ()
   (define-key python-mode-map (kbd "C-c p s") 'python-isort)
   (define-key python-mode-map (kbd "C-c p r") 'python-remove-all-unused-imports)))

;; Program Useful text/config files
;;(use-package json-mode)
;;(use-package markdown-mode)
(use-package protobuf-mode)

;; Useful Tools
;;(use-package quickrun)                  ; quickrun code
;; (use-package restclient                 ; restclient support
;;   :mode (("\\.http\\'" . restclient-mode)))

;; Specific Languages
;;(require 'init-autocomplete)

;; php
(use-package company-php)
(add-hook 'php-mode-hook
          (lambda ()
             (company-mode t)
             (ac-php-core-eldoc-setup)
             (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-ac-php-backend)
))


;;ocaml
;;(require 'init-lang-ocaml)

(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
