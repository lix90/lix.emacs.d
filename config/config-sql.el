;;; config-sql.el --- Configuration for sql development:

;;; Commentary:

;;; Code:
(use-package sql :ensure t :defer t
  :mode (("\\.sql$" . sql-mode)
         ("\\.sqltmpl$" . sql-mode))
  :config
  (setq sql-mysql-login-params
        '((user :default "lixiang")
          (database :default "python_class")
          (server :default "rm-bp17w76ltkok4t0r9po.mysql.rds.aliyuncs.com")
          (port :default 3306)
          (password :default "")))

  (add-hook 'sql-interactive-mode-hook
            (lambda()
              (toggle-truncate-lines t)
              (setq-local show-trailing-whitespace nil)))

  (add-hook 'sql-mode-hook
            (lambda()
              (company-mode t)))

  (use-package sql-indent :ensure t :defer t :after sql
    :init
    (setq sql-indent-offset 2))

  (use-package sqlup-mode :ensure t :defer t
    :init
    (add-hook 'sql-mode-hook 'sqlup-mode)
    (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
    :config
    (add-to-list 'sqlup-blacklist "type"))
  )





;; (use-package sqli :ensure t :defer t)
;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client
;; https://github.com/tmtxt/.emacs.d/blob/master/config/tmtxt-sql.el
(use-package edbi :ensure t :defer t :disabled t)
(use-package company-edbi :ensure t :defer t :disabled t)

;;; SQL interactive mode


;; (setq sql-connection-alist
;;       '((mysql-business (sql-product 'mysql)
;;                         (sql-port 3306)
;;                         (sql-server "rm-bp17w76ltkok4t0r9po.mysql.rds.aliyuncs.com")
;;                         (sql-user "lixiang")
;;                         (sql-database "business"))
;;         ))

;; (defun my-sql-connect (product connection)
;;   ;; load the password
;;   (require my-password "my-password.el.gpg")

;;   ;; update the password to the sql-connection-alist
;;   (let ((connection-info (assoc connection sql-connection-alist))
;;         (sql-password (car (last (assoc connection my-sql-password)))))
;;     (delete sql-password connection-info)
;;     (nconc connection-info `((sql-password ,sql-password)))
;;     (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
;;     (add-to-list 'sql-connection-alist connection-info))

;;   ;; connect to database
;;   (setq sql-product product)
;;   (sql-connect connection))

(provide 'config-sql)
;;; config-sql.el ends here
