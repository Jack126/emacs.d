;;; init-lang.el --- configuration for IDE programming -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; 编程模式下建议开启的一些设置
(defun prog-extra-modes()
  "Extra modes when in programming mode."

  (column-number-mode)
  (display-line-numbers-mode)
  (electric-pair-mode)
  (flymake-mode)
  (hs-minor-mode)
  (prettify-symbols-mode))
(add-hook 'prog-mode-hook 'prog-extra-modes)

;; Flymake
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

;; Golang
(use-package go-mode)
;; lua
(use-package lua-mode)

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
  "FIND the EXEC-FILE and RUN the BODY.  COMMAND ARG-STRING."

  (if (not (executable-find command))
      (message "[ERROR]: <%s> not found!" command)
    (save-buffer)
    (shell-command (format "%s %s" command arg-string))
    (revert-buffer t t t)))

;; BE CAREFUL! Maybe bugs here, always call this function manually.
;; autoload
(defun python-isort ()
  "Sort the imports with isort."
  (interactive)
  (check-run-command "isort"
		             (format "--atomic --profile=black %s"
			                 (buffer-file-name))))

;; BE CAREFUL! Maybe bugs here, always call this function manually.
;; autoload
(defun python-remove-all-unused-imports ()
  "Remove all the unused imports, do NOT use pyimport, as it has bugs.
eg.from datetime import datetime."
  (interactive)
  (check-run-command "autoflake"
		             (format "-i --remove-all-unused-imports %s"
			                 (buffer-file-name))))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c p s") 'python-isort)
            (define-key python-mode-map (kbd "C-c p r") 'python-remove-all-unused-imports)))

;; Program Useful text/config files
(use-package protobuf-mode)


;; php
;; 切换php-mode web-mode
(defun toggle-php-flavor-mode ()
  (interactive)
  "Toggle mode between PHP & Web-Mode Helper modes"
  (cond ((string= mode-name "PHP")
         (web-mode))
        ((string= mode-name "Web")
         (php-mode))))

(use-package company-php)
(add-hook 'php-mode-hook
          (lambda ()
            (define-key php-mode-map (kbd "C-c p w") 'toggle-php-flavor-mode)
            (company-mode t)
            (ac-php-core-eldoc-setup)
            (make-local-variable 'company-backends)
            (add-to-list 'company-backends 'company-ac-php-backend)
            ))

;; Language Server
;; **************************************************
;; `eglot', a light-weight LSP client
(require 'init-eglot)


;; ocaml
(require 'init-ocaml)

;; rust
(use-package rust-mode
    :config
    (define-key rust-mode-map (kbd "C-c f") 'rust-format-buffer))


(provide 'init-lang)

;;; init-lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
