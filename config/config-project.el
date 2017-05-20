(use-package projectile :ensure t :defer t
  :config
  (projectile-global-mode +1)
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package counsel-projectile :ensure t :defer t
  :init
  (counsel-projectile-on))

(use-package find-file-in-project :ensure t :defer t)

(provide 'config-project)
