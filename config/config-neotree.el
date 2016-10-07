
;;; neotree setup
(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle)
  :config (progn
            (setq neo-theme 'nerd)))

(provide 'config-neotree)
;;; config-neotree.el ends here
