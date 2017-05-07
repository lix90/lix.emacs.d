;;; sql
(use-package sql :ensure t :defer t
  :mode (("\.sql$" . sql-mode)
         ("\.sqltmpl$" . sql-mode))
  :config
  (use-package sql-indent :ensure t
	:config
	(setq sql-indent-offset 2))
  (use-package sqlup-mode :ensure t
	:config
	(add-hook 'sql-mode-hook 'sqlup-mode)
	(add-hook 'sql-interactive-mode-hook 'sqlup-mode)))


(provide 'config-sql)
;;; config-sql.el ends here
