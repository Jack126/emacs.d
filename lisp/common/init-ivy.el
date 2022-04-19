
;; (use-package ivy 
;;   :config 
;;   (ivy-mode 1) 
;;   (setq ivy-use-virtual-buffers t 
;;         ivy-initial-inputs-alist nil 
;;         ivy-count-format "%d/%d " 
;;         enable-recursive-minibuffers t 
;;         ivy-re-builders-alist '((t . ivy--regex-ignore-order)))) 

;; (use-package counsel 
;;   :after (ivy) 
;;   :bind (("M-x" . counsel-M-x) 
;;          ("C-x C-f" . counsel-find-file) 
;;          ("C-c g" . counsel-git))) 

;; (use-package swiper 
;;   :after ivy 
;;   :bind (("C-s" . swiper) 
;;          ("C-r" . swiper-isearch-backward)) 
;;   :config (setq swiper-action-recenter t 
;;                 swiper-include-line-number-in-search t))

;;MorefriendlydisplaytransformerforIvy
;; (use-package ivy-rich
;; :config
;;     (ivy-rich-mode 1)
;;     (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
;;     (ivy-rich-modify-columns
;;         'ivy-switch-buffer
;;         '((ivy-rich-switch-buffer-size (:align right))
;;         (ivy-rich-switch-buffer-major-mode (:width 20 :face error)))))

(provide 'init-ivy)