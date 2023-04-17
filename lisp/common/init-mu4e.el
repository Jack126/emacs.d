;;; init-system.el --- configs for startup -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:
(require 'auth-source);; probably not necessary
(setq auth-sources '("~/.authinfo"))

(setq 
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      ;;smtpmail-smtp-user "jack92_liang@163.com"
      smtpmail-smtp-server "smtp.163.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      user-full-name  "Jack Liang"
      user-mail-address "jack92_liang@163.com")
;;Debug
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)


(provide 'init-mu4e)
;;; init-fn.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End: