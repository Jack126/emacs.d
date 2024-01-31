;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-
;;; Code:

;; Misc configurations for default
(setq-default cursor-type 'bar
              fill-column 72 ;; RFC2822 Style
              frame-title-format "🎫 %b %p"
              indent-tabs-mode nil ;; Use space for indent
              isearch-allow-motion t
              isearch-lazy-count t
              load-prefer-newer t
              mode-line-compact t ;; Use compact modeline style
              read-file-name-completion-ignore-case t
              ring-bell-function 'ignore
              tab-width 4 ;; Tab width
              truncate-lines nil
              truncate-partial-width-windows nil
              use-short-answers t ;; Use y/n for yes/no case
              )

;; auto-fill-mode, Help by command or variable name
(add-hook 'after-init-hook 'auto-fill-mode)

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; auto save to the visited file
(add-hook 'after-init-hook 'auto-save-visited-mode)

;; Delete Behavior
;; `delete-selection-mode' is provided by delsel.el (builtin)
;; `delete-trailing-whitespace' is provided by simple.el (builtin)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; fido-mode
;; `fido-mode' is provided by icomplete.el
(add-hook 'after-init-hook 'fido-vertical-mode)
;; customized
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t
      completions-detailed t
      completions-format 'one-column)

;; Follow Mode - Continue reading with parallel buffer
(add-hook 'after-init-hook 'follow-mode)

;; Highlight Current Line
(add-hook 'after-init-hook 'global-hl-line-mode)

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;; minibuffer
(add-hook 'after-init-hook 'minibuffer-electric-default-mode)

;; pulse the cursor line
(dolist (cmd '(recenter-top-bottom other-window))
  (advice-add cmd :after
              (lambda (&rest _) (pulse-momentary-highlight-one-line (point)))))

;; Repeat Mode (builtin from 28)
(add-hook 'after-init-hook 'repeat-mode)

;; Show Paren Mode
(setq show-paren-when-point-in-periphery t
      show-paren-when-point-inside-paren t
      show-paren-style 'mixed)

;; global visual line mode
(add-hook 'after-init-hook 'global-visual-line-mode)

;; auto-highlight-symbol
(add-hook 'after-init-hook 'global-auto-highlight-symbol-mode)

(provide 'init-builtin)

;;; init-builtin.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
