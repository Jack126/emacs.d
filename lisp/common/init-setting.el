;; 切换php-mode web-mode
(defun toggle-php-flavor-mode ()
  (interactive)
  "Toggle mode between PHP & Web-Mode Helper modes"
  (cond ((string= mode-name "PHP")
         (web-mode))
        ((string= mode-name "Web")
         (php-mode))))

;; hello message
(defun my-show-scratch-buffer-message ()
  "Show something in scratch buffer."
  (let* ((fortune-prog (or (executable-find "fortune-zh")
                           (executable-find "fortune"))))
    (cond
     (fortune-prog
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; " ; comment each line
        (replace-regexp-in-string
         "\\(\n$\\|\\|\\[m *\\|\\[[0-9][0-9];?[0-9]?m *\\|\\[;m\\)" ""    ; remove trailing line break
         (shell-command-to-string fortune-prog)))))
     (t
      (concat ";; Happy hacking "
              (or user-login-name "")
              " - Emacs loves you!\n\n")))))

(setq-default initial-scratch-message (my-show-scratch-buffer-message))

;; line number
(setq display-line-numbers-type `relative)
;;显示时间、星期、日期
(display-time-mode 1) ;; 常显
(setq display-time-day-and-date t)
;; 使用 X 剪贴板
(setq x-select-enable-clipboard t)

;; 关闭提示音
(setq visible-bell t)
;; Speedbar
(setq speedbar-show-unknown-files t)
;; weather
(defun tianqi ()
  "天气预报 based on https://github.com/chubin/wttr.in"
  (interactive)
  (eww "zh-cn.wttr.in/qingdao?TAFm")) ;;qingdao,laoshan?TAFm 带区县也是可以

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
  "Replacement for the comment-dwim command. 
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line. Replaces default behaviour of comment-dwim, 
when it inserts comment at the end of the line."
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

;; todo关键词 上色
(add-hook 'prog-mode-hook
    (lambda ()
    (font-lock-add-keywords
        nil '(("\\<\\(FIXME\\|DEBUG\\|TODO\\):"
            1 font-lock-warning-face prepend)))))
;; show todo list
(defun show-todo-list()
"show todo list"
  (interactive)
  (project-find-regexp "TODO")
)
;; 显示标题栏文件路径
(defun show-file-name ()
 "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name))
)
;;winum
(setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-`") 'winum-select-window-by-number)
      (define-key map (kbd "C-²") 'winum-select-window-by-number)
      (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
      (define-key map (kbd "M-1") 'winum-select-window-1)
      (define-key map (kbd "M-2") 'winum-select-window-2)
      (define-key map (kbd "M-3") 'winum-select-window-3)
      (define-key map (kbd "M-4") 'winum-select-window-4)
      (define-key map (kbd "M-5") 'winum-select-window-5)
      (define-key map (kbd "M-6") 'winum-select-window-6)
      (define-key map (kbd "M-7") 'winum-select-window-7)
      (define-key map (kbd "M-8") 'winum-select-window-8)
      map))
;; set neotree the number 0
(defun winum-assign-0-to-neotree ()
  (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))
(add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree)

(global-set-key (kbd "M-;") 'qiang-comment-dwim-line) 
(global-set-key (kbd "M-q") 'query-replace)
(global-set-key (kbd "M-o") 'other-window) ;; other-window
(global-set-key (kbd "M-,") 'pop-tag-mark) ;;previous-buffer
(global-set-key (kbd "C-c ,") 'user) ;; user - Date
(global-set-key (kbd "C-c d") 'copy-line) ;; duplicate-line ;; (M-y) 粘贴
(global-set-key (kbd "C-c f")  'format-all-buffer) ;;格式化代码（prettier）
(global-set-key (kbd "C-c C-j")  'imenu) ;;显示本文件 类名，方法 
(global-set-key [f1] 'manual-entry) ;;
(global-set-key [C-f1] 'info ) ;; 
(global-set-key [f9] 'list-bookmarks) ;;列出所有书签
(global-set-key [f12] 'calendar) ;;日历（init-calendar 详细说明）

(global-set-key [home] 'beginning-of-buffer) ;;设置home键指向buffer开头，end键指向buffer结尾
(global-set-key [end] 'end-of-buffer) ;;文件末尾
(global-set-key [f5] 'toggle-php-flavor-mode) ;;切换php，web模式
(global-set-key (kbd "C-c t") 'show-todo-list) ;;展示所有todo标签
(global-set-key (kbd "C-c a") 'wy-go-to-char) ;;go-to-char 
(global-set-key (kbd "C-c z") 'show-file-name) ;; 展示本文件物理地址
(global-set-key (kbd "C-c w") 'tianqi) ;;查看天气
;;(global-set-key (kbd "<f8>") #'speedbar)
(global-set-key (kbd "C-c o") 'crux-smart-open-line) ;;当前行下插入一行
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line) ;; 替代默认C-a，回到行首
(provide 'init-setting)