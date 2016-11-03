
;;; neotree setup
(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle)
  :config (progn
            (setq neo-theme 'nerd
                  neo-window-width 30
                  neo-window-fixed-size nil)))

(provide 'config-neotree)
;;; config-neotree.el ends here
