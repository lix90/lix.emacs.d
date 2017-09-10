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

(provide 'config-sql)
;;; config-sql.el ends here
