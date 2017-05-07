(use-package projectile :ensure t :defer t
  :init
  (add-hook 'after-init-hook #'projectile-global-mode)
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package counsel-projectile :ensure t :defer t
  :init
  (counsel-projectile-on))

(use-package find-file-in-project :ensure t :defer t)

(provide 'config-project)
