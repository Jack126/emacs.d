;; M-x user
(defun user()
(interactive)
( insert (concat "Author: "
                 (user-login-name) "\n"
                 "Date: "
                 (current-time-string)
                 )))

;; 复制当前行 
(defun copy-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save
     (point)
     (line-end-position)))
     (message "1 line copied"))

;; 添加行注释
(defun qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and current line is not blank and we are not at the end of the line, then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))


(global-set-key (kbd "M-;") 'qiang-comment-dwim-line) 
(global-set-key (kbd "M-q") 'query-replace)
(global-set-key (kbd "M-o") 'other-window) ;; other-window
(global-set-key (kbd "M-,") 'pop-tag-mark) ;;previous-buffer

(global-set-key (kbd "C-c ,") 'user) ;; user - Date
(global-set-key (kbd "C-d") 'copy-line) ;; duplicate-line ;; (M-y) 粘贴
(global-set-key (kbd "C-c f")  'format-all-buffer)


(provide 'init-setting)