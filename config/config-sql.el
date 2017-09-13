;;; sql
(use-package sql :ensure t :defer t
  :mode (("\.sql$" . sql-mode)
         ("\.sqltmpl$" . sql-mode)))

(use-package sql-indent :ensure t :defer t
  :config (setq sql-indent-offset 2))

(use-package sqlup-mode :ensure t :defer t
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  (add-to-list 'sqlup-blacklist "type"))

;; (use-package sqli :ensure t :defer t)
;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client
;; https://github.com/tmtxt/.emacs.d/blob/master/config/tmtxt-sql.el
(use-package edbi :ensure t :defer t)

(provide 'config-sql)
;;; config-sql.el ends here
