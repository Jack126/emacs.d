;;; init-system.el --- configs for startup -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;;; flymake cannot find load-path solution
;; [refs] https://emacs-china.org/t/flymake/8323/19
(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

;;; system coding
;; although others may add many other settings here,
;; but I think the next line is enough
(prefer-coding-system 'utf-8)
;; (set-language-environment "UTF-8")

;;; emacs settings
(setq auto-window-vscroll nil
      help-window-select t
      inhibit-startup-screen t	   ; disable the startup screen splash
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      make-backup-files nil             ; disable backup file
      read-process-output-max (* 64 1024)
      scroll-conservatively 10000
      visible-bell nil)

(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode)
  (pixel-scroll-mode))

;; 显示标题栏文件路径
(defun show-file-name ()
 "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name))
)
(global-set-key (kbd "C-c z") 'show-file-name)

;; line number
(setq display-line-numbers-type `relative)
;;显示时间、星期、日期
(display-time-mode 1) ;; 常显
(setq display-time-day-and-date t) 
;; 使用 X 剪贴板
(setq x-select-enable-clipboard t)

;; todo关键词 上色
(add-hook 'prog-mode-hook
    (lambda ()
    (font-lock-add-keywords
        nil '(("\\<\\(FIXME\\|DEBUG\\|TODO\\):"
            1 font-lock-warning-face prepend)))))
(provide 'init-system)
;;; init-system.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
