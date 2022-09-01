;;; Commentary:
;;; Code:

;;(use-package pos-tip)
;;(use-package fuzzy)
(use-package auto-complete
  :config
  ;; load auto-complete
  (require 'auto-complete-config)
  (ac-config-default)
  ;;(setq ac-quick-help-prefer-pos-tip t)
  ;;(setq ac-use-quick-help t)
  ;;(setq ac-quick-help-delay 0.5)  
  (setq ac-auto-show-menu 0.8)
  ;;  - TAB will behave as RET only on candidate remains
  (setq ac-dwim t)                        
  ;; give a key to trigger ac when it is not automatically triggered
  (ac-set-trigger-key "<C-return>")
  ;; use fuzzy mode, its interesting
  ;;(setq ac-fuzzy-enable t)
  ;; by default we use 3 to start ac
  (setq ac-auto-start 3))
  (setq ac-menu-height 20)
  (auto-complete-mode t)

  (provide 'init-autocomplete)