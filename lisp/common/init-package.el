;;; init-package.el --- initialize the plugins

;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

(use-package company
  :hook (prog-mode . company-mode)
  :config (setq company-minimum-prefix-length 1
                company-show-quick-access t))

;; crux, a collection of many useful extensions/commands
;; without key-binding you can use
;; C-a for its original definition
;; M-m to the indentation of current line
;; C-M-<ARROW> for duplicate lines
;; crux commands? Pls use M-x.
(use-package crux
  :defer 1
  :bind ("C-k" . crux-smart-kill-line))

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
(use-package exec-path-from-shell
  :defer nil
  :when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(windows-nt dos))
              (daemonp)))
  :init (exec-path-from-shell-initialize))

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all
  :defer 1
  :diminish
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

;; gnu-elpa-keyring-update
(use-package gnu-elpa-keyring-update
  :defer 1
  :defer 1)

;; iedit - edit same text in one buffer or region
(use-package iedit
  :defer 1)

;; info-colors, make the info manual as colorful
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;; move-dup, move/copy line or region
(use-package move-dup
  :defer 1
  :hook (after-init . global-move-dup-mode))

;; neotree, file tree manager
(use-package neotree
  :defer 1
  :commands (neo-buffer--lock-width neo-buffer--unlock-width)
  :config (setq neo-autorefresh t
		        neo-theme 'nerd
		        neo-click-changes-root t
		        neo-smart-open t
                )
  :bind ("<f8>" . neotree-toggle))

;; org-superstar
;; make the org mode more beautiful with optimized leading chars
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config (setq org-superstar-prettify-item-bullets t))

;; popwin
(use-package popwin
  :defer 1
  :hook (after-init . popwin-mode))

;; Settings for which-key - suggest next key
(use-package which-key
  :defer 1
  :diminish
  :hook (after-init . which-key-mode))

;; Settings for yasnippet
(use-package yasnippet
  :defer 1
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all)
  :bind ("C-o" . yas-expand))
(add-to-list 'load-path
              "~/.emacs.d/snippets")
(use-package yasnippet-snippets 
  :defer 1
  :diminish)

;; auto-save
(add-to-list 'load-path "~/.emacs.d/modules/auto-save/") ; add auto-save to your load-path
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
;;; custom predicates if you don't want auto save.
;;; disable auto save mode when current filetype is an gpg file.
(setq auto-save-disable-predicates
      '((lambda ()
      (string-suffix-p
      "gpg"
      (file-name-extension (buffer-name)) t))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; 括号高亮
(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))
;; 变量高亮
(use-package rainbow-identifiers
  :hook ((prog-mode emacs-lisp-mode) . rainbow-identifiers-mode))

;; 测试启动时间
;; (use-package esup
;;   :ensure t
;;   ;; To use MELPA Stable use ":pin melpa-stable",
;;   :pin melpa)

;; youdao
(use-package youdao-dictionary)
;; youdao dictionary
(defconst *youdao-dictionary-key* "C-c y")
;; Translation result display scheme, optional postframe, tootip, popup-tip
(defconst *youdao-dictionary-result-display-scheme* 'postframe)
(global-set-key
 (kbd *youdao-dictionary-key*)
 #'(lambda ()
    (interactive)
    (if (display-graphic-p)
      (cond
       ((eq *youdao-dictionary-result-display-scheme* 'tooltip)
	(youdao-dictionary-search-at-point-tooltip))
       ((eq *youdao-dictionary-result-display-scheme* 'postframe)
	(youdao-dictionary-search-at-point-posframe))
       ((eq *youdao-dictionary-result-display-scheme* 'popup-tip)
	(youdao-dictionary-search-at-point+)))
      (youdao-dictionary-search-at-point+))))

;; auto-highlight-symbol
(use-package auto-highlight-symbol)

;;; winum
(use-package winum
  :defer 1)
(winum-mode)


(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
