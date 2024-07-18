;;; Commentary:

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


(setq 
    ;;display-line-numbers-type `relative ;; line number
    display-time-mode 1 ;;显示时间、星期、日期  常显
    display-time-day-and-date t
    x-select-enable-clipboard t ;; 使用 X 剪贴板
    visible-bell t ;; 关闭提示音
    speedbar-show-unknown-files t ;; Speedbar
    speedbar-directory-unshown-regexp "^$"
    kill-ring-max 200 ;;设置粘贴缓冲条目数量.用一个很大的kill ring(最多的记录个数). 这样防止我不小心删掉重要的东西
)

;;eww
(setq
 browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
 shr-use-fonts  nil                          ; No special fonts
 shr-use-colors nil                          ; No colours
 shr-indentation 2                           ; Left-side margin
 shr-width 70                                ; Fold text to 70 columns
 eww-search-prefix "https://wiby.me/?q=")    ; Use another engine for searching

(define-key global-map (kbd  "M-f")
  (lambda ()
    (interactive)
    (forward-word)
    (forward-word)
    (backward-word)))

;; weather
(defun weather ()
  "天气预报 based on https://github.com/chubin/wttr.in"
  (interactive)
  (eww "zh-cn.wttr.in/qingdao,laoshan?TAFm")) ;;qingdao,laoshan?TAFm 带区县也是可以

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

;; 打开默认emacs.d 目录
(defun open-myemacs-dir()
  "Open myown emacs.d directory config file."
  (interactive)
  (dired "~/.emacs.d/"))


(global-set-key (kbd "M-;") 'qiang-comment-dwim-line);; 添加行注释
(global-set-key (kbd "M-q") 'query-replace);;字符查找替换
(global-set-key (kbd "M-o") 'other-window) ;; other-window
(global-set-key (kbd "M-,") 'previous-buffer) ;;previous-buffer

(global-set-key (kbd "C-c ,") 'user) ;; user - Date
(global-set-key (kbd "C-c d") 'copy-line) ;; duplicate-line ;; (M-y) 粘贴
(global-set-key (kbd "C-c f")  'format-all-buffer) ;;格式化代码（prettier）
(global-set-key (kbd "C-c C-j")  'imenu) ;;显示本文件 类名，方法
(global-set-key (kbd "C-c t") 'show-todo-list) ;;展示所有todo标签
(global-set-key (kbd "C-c z") 'show-file-name) ;; 展示本文件物理地址
(global-set-key (kbd "C-c w") 'weather) ;;查看天气
(global-set-key (kbd "C-c o") 'crux-smart-open-line) ;;当前行下插入一行
(global-set-key (kbd "C-c [") 'project-find-file) ;;项目中查找文件
(global-set-key (kbd "C-c ;") 'crux-duplicate-and-comment-current-line-or-region) ;;复制注释当前行

(global-set-key [f1] 'manual-entry) ;;
(global-set-key [C-f1] 'info ) ;;
(global-set-key [f2] 'open-myemacs-dir) ;;打开emacs.d配置 init.el文件
;;(global-set-key [f5] 'mu4e) ;;打开邮件
(global-set-key [f6] 'project-switch-project) ;;打开项目目录
;;(global-set-key (kbd "<f8>") #'neotree)
(global-set-key (kbd "<f8>") #'speedbar)
(global-set-key [f9] 'list-bookmarks) ;;列出所有书签
(global-set-key [f12] 'calendar) ;;日历（init-calendar 详细说明）

(global-set-key [home] 'beginning-of-buffer) ;;设置home键指向buffer开头，end键指向buffer结尾
(global-set-key [end] 'end-of-buffer) ;;文件末尾

(global-set-key (kbd "C-a") 'crux-move-beginning-of-line) ;; 替代默认C-a，回到行首

(global-set-key (kbd "C-c 9") 'scroll-other-window);;其他window向下翻页
(global-set-key (kbd "C-c 0") 'scroll-other-window-down);; 向上翻

(global-set-key (kbd "C-c b") 'blink-search);; blink-search

(provide 'init-setting)
