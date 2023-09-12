;;; init-package.el --- initialize the plugins

;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

(use-package company
  :hook ((prog-mode . company-mode)
         (inferior-emacs-lisp-mode . company-mode))
  :config (setq company-minimum-prefix-length 1
                company-show-quick-access nil))

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


;; iedit - edit same text in one buffer or region
;; (use-package iedit
;;   :defer 1)

;; info-colors, make the info manual as colorful
;; (use-package info-colors
;;   :hook (Info-selection . info-colors-fontify-node))

;; move-dup, move/copy line or region
(use-package move-dup
  :defer 1
  :hook (after-init . global-move-dup-mode))

;; neotree, file tree manager
;; (use-package neotree
;;   :defer 1
;;   :commands (neo-buffer--lock-width neo-buffer--unlock-width)
;;   :config (setq neo-autorefresh t
;; 		        neo-theme 'nerd
;; 		        neo-click-changes-root t
;; 		        neo-smart-open t
;;                 )
;;   :bind ("<f8>" . neotree-toggle))

;; org-superstar
;; make the org mode more beautiful with optimized leading chars
;; (use-package org-superstar
  ;; :hook (org-mode . org-superstar-mode)
  ;; :config (setq org-superstar-prettify-item-bullets t))

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
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
;; 变量高亮
(use-package rainbow-identifiers
  :hook ((prog-mode emacs-lisp-mode) . rainbow-identifiers-mode))

;; 测试启动时间
;; (use-package esup
;;   :ensure t
;;   ;; To use MELPA Stable use ":pin melpa-stable",
;;   :pin melpa)


;; auto-highlight-symbol
(use-package auto-highlight-symbol)

;;; winum
(use-package winum
  :defer 1)
(winum-mode)

;; Org Mode
(use-package org
  :ensure nil
  :config
  (setq org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-startup-indented t
        org-latex-listings 'minted
        ;; use tectonic to export pdf
        org-latex-pdf-process '("tectonic -Z shell-escape %f"))
  ;; solve CJK issue when export to pdf
  (add-to-list 'org-latex-packages-alist '("" "ctex"))
  ;; highlight code block
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; long word wrap when export to pdf
  (add-to-list 'org-latex-packages-alist '("" "seqsplit")))

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
  :load-path "~/.emacs.d/modules/achive"
  :bind
  ("C-c a a" . achive)
  :custom
  (achive-auto-refresh t)
  (achive-refresh-seconds 5)
  (achive-stock-list '("sz002317" "sz000400" "sh600438" "sh600703")))


;; (use-package markdown-mode
;;   :ensure t
;;   :mode ("README\\.md\\'" . gfm-mode)
;;   :init (setq markdown-command "multimarkdown")
;;   :bind (:map markdown-mode-map
;;          ("C-c C-e" . markdown-do)))

(use-package tablist
    :defer 1)


;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
