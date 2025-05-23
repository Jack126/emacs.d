;;; init-programming.el --- configurations for Programmers
;;; Commentary:
;;; Code:

;;python
(defun check-run-command (command arg-string)
  "FIND the EXEC-FILE and RUN the BODY.  COMMAND ARG-STRING."

  (if (not (executable-find command))
      (message "[ERROR]: <%s> not found!" command)
    (save-buffer)
    (shell-command (format "%s %s" command arg-string))
    (revert-buffer t t t)))

;; BE CAREFUL! Maybe bugs here, always call this function manually.
;; autoload
(defun python-isort ()
  "Sort the imports with isort."
  (interactive)
  (check-run-command "isort"
		             (format "--atomic --profile=black %s"
			                 (buffer-file-name))))

;; BE CAREFUL! Maybe bugs here, always call this function manually.
;; autoload
(defun python-remove-all-unused-imports ()
  "Remove all the unused imports, do NOT use pyimport, as it has bugs.
eg.from datetime import datetime."
  (interactive)
  (check-run-command "autoflake"
		             (format "-i --remove-all-unused-imports %s"
			                 (buffer-file-name))))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c p s") 'python-isort)
            (define-key python-mode-map (kbd "C-c p r") 'python-remove-all-unused-imports)))


(provide 'init-programming)

;;; init-programming.el ends here
