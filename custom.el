(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(restclient quickrun yaml-mode protobuf-mode markdown-mode json-mode typescript-mode web-mode emmet-mode rust-mode go-mode eglot yasnippet-snippets yasnippet which-key highlight-parentheses rainbow-delimiters popwin org-superstar neotree move-dup info-colors iedit gnu-elpa-keyring-update exec-path-from-shell crux ctrlf company auto-package-update format-all delight diminish use-package)))
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

;; 复制当前行 
(defun copy-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save
     (point)
     (line-end-position)))
     (message "1 line copied"))

(global-set-key (kbd "M-q") 'query-replace)
(global-set-key (kbd "C-c .") 'now) ;; now time
(global-set-key (kbd "C-c ,") 'user) ;; user
(global-set-key (kbd "M-o") 'other-window) ;; other-window
(global-set-key (kbd "C-d") 'copy-line) ;; duplicate-line ;; (M-y) 粘贴
(global-set-key (kbd "M-,") 'pop-tag-mark) ;;previous-buffer
(global-set-key (kbd "C-c f")  'format-all-buffer)
(global-set-key (kbd "C-c c f")  'counsel-find-file)
(global-set-key (kbd "C-c c r")  'counsel-rg)
(global-set-key (kbd "C-c s")  'save-buffer)

