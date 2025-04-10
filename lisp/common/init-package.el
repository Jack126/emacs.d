;;; init-package.el --- initialize the plugins

;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

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

;; move-dup, move/copy line or region
(use-package move-dup
  :defer 1
  :hook (after-init . global-move-dup-mode))

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; 括号高亮
(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
;; 变量高亮
; (use-package rainbow-identifiers
;   :hook ((prog-mode emacs-lisp-mode) . rainbow-identifiers-mode))

;; 测试启动时间
;; (use-package esup
;;   :ensure t
;;   ;; To use MELPA Stable use ":pin melpa-stable",
;;   :pin melpa)


;; auto-highlight-symbol
(use-package auto-highlight-symbol)

;; windmove.el, use  <SHIFT - arrow key> to switch buffers
; (use-package windmove
;   :ensure t
;   :config (windmove-default-keybindings)
;   :bind ("M-1" . windmove-left)
;   :bind ("M-2" . windmove-down)
;   :bind ("M-3" . windmove-up)
;   :bind ("M-4" . windmove-right))

;; dashboard
; (use-package dashboard
;   :custom
;   (dashboard-center-content 1)
;   (dashboard-set-heading-icons t)
;   (dashboard-set-file-icons t)
;   (dashboard-items '((projects . 5)
;                      (recents . 5)
;                      (agenda . 5)
;                      (registers . 5)))
;   :init
;   (dashboard-setup-startup-hook)
 
;   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))
; (setq dashboard-startup-banner "~/.emacs.d/banner.txt")
;;(setq dashboard-startup-banner 4)
;; Recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  :bind (("C-c r" . #'recentf-open-files))
  :config
  (setq-default recentf-max-menu-items 50
                recentf-max-saved-items 50)
  (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/")))


;; achive
(use-package achive
  :load-path "~/.emacs.d/extensions/achive"
  :bind
  ("C-c a a" . achive)
  :custom
  (achive-auto-refresh t)
  (achive-refresh-seconds 5)
  (achive-stock-list '("sh600597" "sz000949" "sh600350" "sh600703")))
(use-package tablist
    :defer 1)

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md$" . markdown-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; blink-search
(add-to-list 'load-path "~/.emacs.d/extensions/blink-search")
(require 'blink-search)

;; sort-tab
(add-to-list 'load-path "~/.emacs.d/extensions/sort-tab") ; add sort-tab to your load-path
(require 'sort-tab)
(sort-tab-mode 1)


;; doom-theme
; (use-package doom-themes
;   :ensure t
;   :config
;   ;; Global settings (defaults)
;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;   (load-theme 'doom-one t)

;   ;; Enable flashing mode-line on errors
;   (doom-themes-visual-bell-config)
;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;   (doom-themes-neotree-config)
;   ;; or for treemacs users
;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;   (doom-themes-treemacs-config)
;   ;; Corrects (and improves) org-mode's native fontification.
;   (doom-themes-org-config))

;;; Require
(use-package disable-mouse)
;;; Code:
(global-disable-mouse-mode)

;; undo tree
(use-package undo-tree)
(global-undo-tree-mode)
(setq undo-limit 800000
      undo-strong-limit 1200000)

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
