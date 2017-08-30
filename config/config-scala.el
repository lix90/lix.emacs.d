(use-package scala-mode :ensure t :defer t)

(use-package ensime :ensure t :defer t
  :config
  (add-hook 'scala-mode-hook 'ensime-mode))

(provide 'config-scala)
