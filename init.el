;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-
;; Author: Cabins
;; Github: https://github.com/cabins-emacs.d
;;; Commentary:
;; (c) Cabins Kong, 2022-
;;; Code:

(defvar cabins-os-win (memq system-type '(ms-dos windows-nt cygwin)))
(defvar cabins-os-mac (eq system-type 'darwin))
(when (find-font (font-spec :family "Sarasa Mono SC"))
  (set-face-attribute 'default nil :family "Sarasa Mono SC"))
(menu-bar-mode -1)
;; packages
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'init-functions)
(require 'init-programming)
(require 'init-setting)

(require 'package)
(setq package-check-signature nil
      load-prefer-newer t)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/"))

;; don't bother with the initialize, although it may cause much startup time,
;; there's no way to avoid this if you use package.el instead of other package
;; manager, like straight.el
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; these code run only once, when use-package is not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)
(require 'use-package)


;; Emacs builtin packages
(setq-default auto-window-vscroll nil
	      default-directory "~"
	      default-text-properties '(line-spacing 0.2 line-height 1.2) ;default line height
	      frame-title-format "%b"
	      help-window-select t
	      initial-major-mode 'fundamental-mode
	      inhibit-startup-screen t ; disable the startup screen splash
	      isearch-allow-motion t
	      isearch-lazy-count t
	      kill-whole-line t
	      mode-line-compact t
	      make-backup-files nil	; disable backup file
	      read-process-output-max (* 4 1024 1024)
	      require-final-newline t
	      scroll-conservatively 1000
	      show-trailing-whitespace t
	      system-time-locale "C"
	      use-short-answers t)

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; auto save to the visited file (provided by `files.el')
;;(add-hook 'after-init-hook 'auto-save-visited-mode)

;; Delete Behavior
;; `delete-selection-mode' is provided by delsel.el (builtin)
(add-hook 'after-init-hook 'delete-selection-mode)

;; visual-line-mode
(add-hook 'after-init-hook 'global-visual-line-mode)

;; fido-vertical-mode
(add-hook 'after-init-hook 'fido-vertical-mode)

;; pixel-scroll-precise-mode
;;(add-hook 'after-init-hook 'pixel-scroll-precision-mode)

;; fido-mode
;; `fido-mode' is provided by icomplete.el
(use-package icomplete
  :hook (after-init . fido-mode)
  :config (setq completions-detailed t))

;; Highlight Current Line
(use-package hl-line
  :when (display-graphic-p)
  :hook (prog-mode . hl-line-mode))

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;; Org Mode
(use-package org
  :hook (org-mode . org-num-mode)
  :config
  (setq org-hide-leading-stars t
	org-hide-emphasis-markers t
	org-startup-indented t))

;; Pulse the cursor line
(dolist (cmd '(recenter-top-bottom other-window))
  (advice-add cmd :after (lambda (&rest _) (pulse-momentary-highlight-one-line))))

;; Show Paren Mode
(use-package paren
  :config
  (setq show-paren-when-point-in-periphery t
	show-paren-when-point-inside-paren t
	show-paren-style 'mixed))

;; Recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  ;; recentf-open since v29.1, recentf-open-files since v22
  :bind (("C-c r" . #'recentf-open-files)))

;; windmove.el, use  <SHIFT - arrow key> to switch buffers
(use-package windmove
  :config (windmove-default-keybindings))

;; Settings for which-key - suggest next key, Builtin package now
(use-package which-key
  :hook (after-init . which-key-mode))

;; Third part packages
;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-enable-imenu-support t
      use-package-expand-minimally t)

;; Settings for company, auto-complete only for coding.
(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
(use-package exec-path-from-shell
  :ensure t
  :when (or (memq window-system '(mac ns x))
	    (unless cabins-os-win
	      (daemonp)))
  :init (exec-path-from-shell-initialize))

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all :ensure t
  ;; enable format on save with format-all-mode
  ;; :hook ((prog-mode . format-all-mode)
  ;; 	   (format-all-mode . format-all-ensure-formatter))
  ;; and bind a shortcut to manual format
  :bind ("C-c f" . #'format-all-region-or-buffer))

;; iedit - edit same text in one buffer or region
(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

;; move-dup, move/copy line or region
(use-package move-dup
  :ensure t
  :hook (after-init . global-move-dup-mode))

;; crux
(use-package crux
  :defer 1
  :bind ("C-k" . crux-smart-kill-line))

;; undo-tree
(use-package undo-tree
  :config
  (setq undo-limit 800000
        undo-strong-limit 1200000))
(global-undo-tree-mode)

;;Configs for OS
;; Special configs for MS-Windows
(when (and cabins-os-win
	   (boundp 'w32-get-true-file-attributes))
  (setq w32-get-true-file-attributes nil
	w32-pipe-read-delay 0
	w32-pipe-buffer-size (* 64 1024)))

;; Special configs for macOS
(when cabins-os-mac
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super
	ns-use-native-fullscreen t))

;; solve the Chinese paste issue
;; let Emacs auto-guess the selection coding according to the Windows/system settings
(prefer-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8)
(unless cabins-os-win
  (set-selection-coding-system 'utf-8))

;; Configs for programming languages
(add-hook 'prog-mode-hook (lambda () (setq-local column-number-mode t)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'which-function-mode)

;; Flymake
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . #'flymake-goto-next-error)
	 ("M-p" . #'flymake-goto-prev-error)))


;; Markdown file support
(use-package markdown-mode
  :ensure t)

;; Settings for yasnippet
(use-package yasnippet
  :hook
  (after-init . yas-global-mode)
  :custom
  (yas-keymap-disable-hook t)
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)))
(add-to-list 'load-path
              "~/.emacs.d/snippets")
(use-package yasnippet-snippets
  :defer 1
  :diminish)

;; auto-save
(add-to-list 'load-path "~/.emacs.d/extensions/auto-save/") ; add auto-save to your load-path
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

;; lsp-bridge
(unless (display-graphic-p)
  (add-to-list 'load-path "~/.emacs.d/extensions/popon")
  (add-to-list 'load-path "~/.emacs.d/extensions/acm-terminal"))

(unless (display-graphic-p)
  (with-eval-after-load 'acm
    (require 'acm-terminal)))
(add-to-list 'load-path "~/.emacs.d/extensions/lsp-bridge")
(require 'lsp-bridge)

(setq lsp-bridge-enable-completion-in-minibuffer t)
(setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
(setq lsp-bridge-enable-with-tramp t)
(setq lsp-bridge-enable-org-babel t)
(setq acm-enable-quick-access t)
(setq acm-backend-yas-match-by-trigger-keyword t)
(setq acm-enable-tabnine nil)
(setq acm-enable-codeium nil)
(setq lsp-bridge-enable-log nil)

(global-lsp-bridge-mode)

(use-package achive
  :load-path "~/.emacs.d/extensions/achive"
  :bind ("C-c a a" . achive)
  :custom
  (achive-auto-refresh t)
  (achive-refresh-seconds 5)
  (achive-stock-list '("sh600597" "sz000949" "sh600350" "sh600703")))

(use-package tablist :defer t)

(use-package sort-tab
  :load-path "~/.emacs.d/extensions/sort-tab"
  :config (sort-tab-mode 1))


(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
