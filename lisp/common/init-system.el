;;; init-system.el --- configs for startup -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:

(require 'subr-x)
(defvar os--windows (memq system-type '(ms-dos windows-nt cygwin)))
(defvar os--macos (eq system-type 'darwin))
(cond
 (os--windows
  ;; fix the 
 issue on Windows
  ;; (setq buffer-file-coding-system 'utf-8)
  (when (boundp 'w32-get-true-file-attributes)
    (setq w32-get-true-file-attributes nil
	      w32-pipe-read-delay 0
	      w32-pipe-buffer-size (* 64 1024))))
 (os--macos
  ;; <macOS> Command -> Meta, Option -> Super
  (setq 
        mac-command-modifier 'meta
	    mac-option-modifier 'super
	    ns-use-native-fullscreen t))
 (t nil))

;; solve the Chinese paste issue
;; let Emacs auto-guess the selection coding according to the Windows/system settings
(unless os--windows
  (set-selection-coding-system 'utf-8))
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun cabins/font-setup ()
  "Font setup."

  (interactive)
  (when (display-graphic-p)
    ;; Default font
    (cl-loop for font in '("JetbrainsMono Nerd Font" "Courier Prime" "Cascadia Code" "Fira Code" "Hack" "Source Code Pro" "Menlo" "Monaco" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil 
                                        :family font
                                        :height (cond ((eq system-type 'darwin) 155)
                                                      (t 100))))

    ;; Unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
             when (font-installed-p font)
             return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

    ;; Chinese characters
    (cl-loop for font in '("霞鹜文楷" "WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei UI" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.2)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

;;;###autoload
(defun make-ui-cleaner ()
  "Remove all the unnecessary elements."

  ;; tooltips in echo-aera
  (when (fboundp 'tooltip-mode) (tooltip-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  ;; for menu-bar, only show when runs on GUI mode & macos
  (when (fboundp 'menu-bar-mode)
    (if (and (display-graphic-p) (eq system-type 'darwin))
      (menu-bar-mode +1)
    (menu-bar-mode -1))))

;;;###autoload
(defun cabins/available-theme (theme-list)
  "Get the first available theme from THEME-LIST."

  (cl-loop for theme in theme-list
           when (member theme (custom-available-themes))
           return theme))

(defun cabins/os-dark-mode()
  "Check the os dark mode, only support Windows for now."

  (let* ((cmd (cond
               ((member system-type '(ms-dos windows-nt cygwin))
                "powershell (Get-ItemProperty -Path HKCU:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize -Name AppsUseLightTheme).AppsUseLightTheme")
               ((eq system-type 'darwin)
                "defaults read -g AppleInterfaceStyle")
               ((eq system-type 'gnu/linux)
                "gsettings get org.gnome.desktop.interface color-scheme")))
         (mode (string-trim (shell-command-to-string cmd))))
    (message mode)
    (if (member mode '("0" "Dark" "'prefer-dark'")) t nil)))

(defun cabins/load-theme()
  "Load theme, Auto change color scheme according to system dark mode on Windows."

  (interactive)
  (when (display-graphic-p)
    (let ((light-theme (cabins/available-theme '(modus-operandi leuven tsdh-light tango whiteboard)))
          (dark-theme (cabins/available-theme '(dracula modus-vivendi leuven-dark tsdh-dark tango-dark wombat dichromacy))))
      (if (cabins/os-dark-mode)
          (load-theme dark-theme t)
        (load-theme light-theme t)))))

(add-hook 'emacs-startup-hook 'cabins/font-setup)

;;; flymake cannot find load-path solution
;; [refs] https://emacs-china.org/t/flymake/8323/19
(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

;;; system coding
;; although others may add many other settings here,
;; but I think the next line is enough
;; (prefer-coding-system 'utf-8)
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

;; Misc configurations for default
(setq-default cursor-type 'bar
              fill-column 72 ;; RFC2822 Style
              frame-title-format "🎫 %b %p"
              indent-tabs-mode nil ;; Use space for indent
              isearch-allow-motion t
              isearch-lazy-count t
              load-prefer-newer t
              mode-line-compact t ;; Use compact modeline style
              read-file-name-completion-ignore-case t
              ring-bell-function 'ignore
              tab-width 4 ;; Tab width
              truncate-lines nil
              truncate-partial-width-windows nil
              use-short-answers t ;; Use y/n for yes/no case
              )

;; auto-fill-mode, Help by command or variable name
(add-hook 'after-init-hook 'auto-fill-mode)

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; auto save to the visited file
(add-hook 'after-init-hook 'auto-save-visited-mode)

;; Delete Behavior
;; `delete-selection-mode' is provided by delsel.el (builtin)
;; `delete-trailing-whitespace' is provided by simple.el (builtin)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; fido-mode
;; `fido-mode' is provided by icomplete.el
(add-hook 'after-init-hook 'fido-vertical-mode)
;; customized
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t
      completions-detailed t
      completions-format 'one-column)

;; Follow Mode - Continue reading with parallel buffer
(add-hook 'after-init-hook 'follow-mode)

;; Highlight Current Line
(add-hook 'after-init-hook 'global-hl-line-mode)

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;; minibuffer
(add-hook 'after-init-hook 'minibuffer-electric-default-mode)

;; pulse the cursor line
(dolist (cmd '(recenter-top-bottom other-window))
  (advice-add cmd :after
              (lambda (&rest _) (pulse-momentary-highlight-one-line (point)))))

;; Repeat Mode (builtin from 28)
(add-hook 'after-init-hook 'repeat-mode)

;; Show Paren Mode
(setq show-paren-when-point-in-periphery t
      show-paren-when-point-inside-paren t
      show-paren-style 'mixed)

;; global visual line mode
(add-hook 'after-init-hook 'global-visual-line-mode)

;; auto-highlight-symbol
(add-hook 'after-init-hook 'global-auto-highlight-symbol-mode)



(provide 'init-system)
;;; init-system.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
