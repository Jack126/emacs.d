;;; init-fn.el --- customized functions -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

;; for debug
;;;###autoload

(defmacro cabins/timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))


(defun cabins/available-font (font-list)
  "Get the first available font from FONT-LIST."

  (cl-loop for font in font-list
           when (member font (font-family-list))
           return font))

(defvar cn-fonts-list '("Hack Nerd Font" "黑体" "STHeiti" "微软雅黑" "文泉译微米黑")
  "定义使用的中文字体候选列表.")

(defvar en-fonts-list '("Hack Nerd Font" "Cascadia Code" "Courier New" "Monaco" "Ubuntu Mono")
"定义使用的英文字体候选列表.")

(defvar emoji-fonts-list '("Hack Nerd Font" "Apple Color Emoji" "Segoe UI Emoji" "Noto Color Emoji" "Symbola" "Symbol")
  "定义使用Emoji字体候选列表.")

;;;###autoload
(defun cabins/font-setup ()
  "Font setup."

  (interactive)
  (let* ((cf (cabins/available-font cn-fonts-list))
	     (ef (cabins/available-font en-fonts-list))
         (em (cabins/available-font emoji-fonts-list)))
    (when ef
      (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
	    (set-face-attribute face nil :family ef :height 150)))
    (when em
      (set-fontset-font t 'emoji em)
      (set-fontset-font t 'symbol em))
    (when cf
      (dolist (charset '(kana han cjk-misc bopomofo))
	    (set-fontset-font t charset cf))
      (setq face-font-rescale-alist
	        (mapcar (lambda (item) (cons item 1.2)) `(,cf ,em))))))
(add-hook 'after-init-hook #'cabins/font-setup)

;;;autoload
(defun tenon--cleaner-ui ()
  "Remove all the unnecessary elements."

  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))

  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))

  (if (and (fboundp 'menu-bar-mode)
           (display-graphic-p))
      (menu-bar-mode +1)
    (menu-bar-mode -1))

  ;; tooltips in echo-aera
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))


(provide 'init-fn)
;;; init-fn.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
