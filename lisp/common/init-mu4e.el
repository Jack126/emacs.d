;;; init-system.el --- configs for startup -*- lexical-binding: t -*-
;;; Commentary:
;; (c) Cabins Kong, 2022-

;;; Code:
;;(use-package 'auth-source);; probably not necessary
(setq auth-sources '("~/.authinfo"))

(setq smtpmail-smtp-user "jack92_liang@163.com"
      smtpmail-smtp-server "smtp.163.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)
;;Debug
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)

;; mu4e
(when (eq system-type 'darwin)
  (add-to-list 'load-path "/usr/local/Cellar/mu/1.6.5/share/emacs/site-lisp/mu/mu4e"))
(require 'mu4e)
(use-package org-mime)
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-maildir "~/workspace/mail/.mail")


;; set default values about mu4e
(setq
 mu4e-get-mail-command "offlineimap -u quiet"  ;; 使用 offlineimap 获取邮件
 ;; auto update maildir with isync and index it
 mu4e-update-interval 60
 ;; don't do a full cleanup check
 mu4e-index-cleanup nil
 ;; don't consider up-to-date dirs
 mu4e-index-lazy-check t
 ;; show images in message mode
 mu4e-view-show-images t
 ;; set the default download dir for attachment
 mu4e-attachment-dir "~/Downloads"
 ;; prefer html view
 mu4e-view-prefer-html t
 ;; don't save message to sent messages, gmail/imap takes care of this
 ;; (see the documentation for `mu4e-sent-messages-behavior' if you have
 ;; additional non-gmail addresses and want assign them different
 ;; behavior.)
 mu4e-sent-messages-behavior 'delete
 )

;; set default values for sending mails
(setq
 ;; user agent when send mail
 mail-user-agent 'mu4e-user-agent
 ;; 设置邮件发送方法为 smtpmail
 message-send-mail-function 'smtpmail-send-it
 ;; 根据 from 邮件头使用正确的账户上下文发送 email.
 message-sendmail-envelope-from 'header
 ;; 设置邮箱认证加密方式
 smtpmail-stream-type 'ssl
 ;; don't keep message buffers around
 message-kill-buffer-on-exit t
 )

;; some information about me
(setq
 user-full-name  "Jack Liang"
 ;; set a mail address list using when reply a message
 mu4e-user-mail-address-list '("jack92_liang@163.com")
 mu4e-compose-signature
 (concat
  "Jack\n"
  "Liang\n")
 )
;; 设置 mu4e 上下文
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "163"
           :enter-func (lambda ()
                         (mu4e-message "Entering Netease context")
                         ;; update index after switch context, otherwise the
                         ;; counting is not updated
                         (mu4e-update-index))
           :leave-func (lambda () (mu4e-message "Leaving Netease context"))
           ;; we match based on the contact-fields of the message
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "163" (mu4e-message-field msg :maildir))))

           :vars '((user-mail-address             . "jack92_liang@163.com")
                   (mu4e-sent-folder              . "/163/INBOX")
                   (mu4e-drafts-folder            . "/163/草稿箱")
                   (mu4e-trash-folder             . "/163/垃圾邮件")
                   (mu4e-refile-folder            . "/163/Refile")
                   (smtpmail-smtp-user            . "jack92_liang@163.com")
                   (smtpmail-default-smtp-server  . "smtp.163.com")
                   (smtpmail-smtp-server          . "smtp.163.com")
                   (smtpmail-smtp-service         . 465)
                   ;;(mu4e-get-mail-command         . "mbsync Netease")
                   (mu4e-maildir-shortcuts . (("/163/INBOX"   . ?i)
                                              ("/163/已发送"    . ?s)
                                              ("/163/Refile"  . ?r)
                                              ("/163/垃圾邮件"   . ?t)
                                              ("/163/草稿箱"  . ?d)))
                   (mu4e-bookmarks . ( ("maildir:/163/INBOX AND flag:unread AND NOT flag:trashed"   "Unread messages"        ?u)
                                       ("maildir:/163/INBOX AND date:today..now"                    "Today's messages"       ?t)
                                       ("maildir:/163/INBOX AND date:7d..now"                       "Last 7 days"            ?w)
                                       ("maildir:/163/INBOX AND date:1d..now"                       "Last 1 days"            ?o)
                                       ("maildir:/163/INBOX"                                        "INBOX"                  ?i)
                                       ("maildir:/163/已发送"                                        "已发送"                   ?s)
                                       ("maildir:/163/Refile"                                       "Refile"                 ?r)
                                       ("maildir:/163/垃圾邮件"                                      "垃圾邮件"                  ?t)
                                       ("maildir:/163/草稿箱"                                        "草稿箱"                 ?d)
                                       ("maildir:/163/INBOX AND mime:image/*"                       "Messages with images"   ?p)))
                   ))

         ))

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first)

(provide 'init-mu4e)
;;; init-fn.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End: