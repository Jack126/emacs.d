(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(super-save restclient quickrun yaml-mode protobuf-mode markdown-mode json-mode typescript-mode web-mode emmet-mode rust-mode go-mode eglot yasnippet-snippets yasnippet which-key highlight-parentheses rainbow-delimiters popwin org-superstar neotree move-dup info-colors iedit gnu-elpa-keyring-update format-all exec-path-from-shell crux ctrlf company auto-package-update delight diminish use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


 ;; M-x now
(defun now ()
(interactive)
( insert (concat "Date: "
                (current-time-string))))

;; M-x user
(defun user()
(interactive)
( insert (concat "Author: "
                 (user-login-name))))

;; copy line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

(global-set-key (kbd "M-r") 'query-replace)
(global-set-key (kbd "C-c .") 'now) ;; now time
(global-set-key (kbd "C-c ,") 'user) ;; user
(global-set-key (kbd "M-o") 'other-window) ;; other-window
(global-set-key (kbd "C-d") 'duplicate-line) ;; duplicate-line
(global-set-key (kbd "M-,") 'pop-tag-mark) ;;previous-buffer

;; line number 
(setq display-line-numbers-type `relative)