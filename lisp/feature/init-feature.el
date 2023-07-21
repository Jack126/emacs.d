;;; init-feature.el --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(advice-add 'load-theme :after
            (lambda (&rest _)
              (set-face-attribute 'mode-line nil :box nil)
              (set-face-attribute 'mode-line-inactive nil :box nil)))

;; UI
;; disable scrollbar/toolbar on all platform
;; keep the menu bar on all UI mode
(add-hook 'window-setup-hook #'tenon--cleaner-ui)
(add-hook 'tty-setup-hook #'tenon--cleaner-ui)

;; daemon
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (cabins/font-setup))))
  (cabins/font-setup))


(provide 'init-feature)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init-feature.el ends here
