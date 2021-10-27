;;; early-init.el --- Emacs 27 introduces early-init.el, which runs before init.el
;;; Commentary:
;; Runs before package and UI initializetion happens.
;;; Code:

;; For speed up the startup, please do NOT forget reset it to default
;; after Emacs after-init-hook, or it may cause freezes.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; Prevent unwanted runtime compilation for native-comp users
(setq native-comp-deferred-compilation nil)

;; Package initialize occurs automatically, before `user-init-file' is loaded
;; but after `early-init-file'. If you want to handle package initialization,
;; you can prevent Emacs from doing it early by uncomment next line!
(setq package-enable-at-startup nil)

;; [From DOOM Emacs]
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)
(setq-default inhibit-redisplay t
	      inhibit-message t)
(add-hook 'window-setup-hook
	  (lambda ()
	    (setq-default inhibit-redisplay nil
			  inhibit-message nil)
	    (redisplay)))

;; System default coding
(set-language-environment "UTF-8")

;; Load theme early to avoid the blinking
(load-theme 'leuven)

;; Cleaner GUI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode 0)

;;; early-init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; END:
