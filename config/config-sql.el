;;; config-sql.el --- Configuration for sql development:

;;; Commentary:

;;; Code:
(use-package sql :ensure t :defer t
  :mode (("\.sql$" . sql-mode)
         ("\.sqltmpl$" . sql-mode))
  :config
  (add-hook 'sql-mode-hook
            (lambda()
              (company-mode t))))

(use-package sql-indent :ensure t :defer t
  :config
  (setq sql-indent-offset 2))

(use-package sqlup-mode :ensure t :defer t
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  (add-to-list 'sqlup-blacklist "type"))

;; (use-package sqli :ensure t :defer t)
;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client
;; https://github.com/tmtxt/.emacs.d/blob/master/config/tmtxt-sql.el
(use-package edbi :ensure t :defer t :disabled t)
(use-package company-edbi :ensure t :defer t :disabled t)

;;; SQL interactive mode
(setq sql-oracle-login-params
      '((user :default "proc_ods_22")
        (database :default "orcl")
        (server :default "192.168.1.210")
        (port :default 1521)
        (password :default "")))

(setq sql-mysql-login-params
      '((user :default "root")
        (database :default "")
        (server :default "localhost")
        (port :default 3306)
        (password :default "")))

(add-hook 'sql-interactive-mode-hook
          (lambda()
            (toggle-truncate-lines t)
            (setq-local show-trailing-whitespace nil)))

(provide 'config-sql)
;;; config-sql.el ends here
