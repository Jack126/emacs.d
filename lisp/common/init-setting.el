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
;; (global-set-key (kbd "C-c c f")  'counsel-find-file)
;; (global-set-key (kbd "C-c c r")  'counsel-rg)
;; (global-set-key (kbd "C-c s")  'save-buffer)

(provide 'init-setting)