;;; early-init.el --- Emacs 27 introduces early-init.el, which runs before init.el
;;; Commentary:
;; Runs before package and UI initializetion happens.
;;; Code:

(unless (>= emacs-major-version 28)
  (error "ONLY EMACS v28+ IS SUPPORTED!"))

;; For speed up the startup, please do NOT forget reset it to default
;; after Emacs after-init-hook, or it may cause freezes.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; Prevent unwanted runtime compilation for native-comp users
(setq inhibit-automatic-native-compilation t)

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

;; Clean GUI
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
;; System default coding
(set-language-environment 'utf-8)

;;(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes/")
;;(load-theme 'zenburn t)
(load-theme 'misterioso t)

;; start time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; early-init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; END: