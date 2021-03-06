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
;;go-to-char 
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
             char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
(define-key global-map (kbd "C-c a") 'wy-go-to-char)


(global-set-key (kbd "M-;") 'qiang-comment-dwim-line) 
(global-set-key (kbd "M-q") 'query-replace)
(global-set-key (kbd "M-o") 'other-window) ;; other-window
(global-set-key (kbd "M-,") 'pop-tag-mark) ;;previous-buffer

(global-set-key (kbd "C-c ,") 'user) ;; user - Date
(global-set-key (kbd "C-c d") 'copy-line) ;; duplicate-line ;; (M-y) 粘贴
(global-set-key (kbd "C-c f")  'format-all-buffer)
(global-set-key (kbd "C-c C-j")  'imenu)



;; youdao dictionary
(defconst *youdao-dictionary-key* "C-c y")

;; Translation result display scheme, optional postframe, tootip, popup-tip
(defconst *youdao-dictionary-result-display-scheme* 'postframe)

(provide 'init-setting)