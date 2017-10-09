(use-package go-mode :ensure t :defer t
  :mode ("\\.go$" . go-mode))

(use-package company-go :ensure t :defer t)
(use-package go-snippets :ensure t :defer t :disabled t)
(use-package go-eldoc :ensure t :defer t
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'config-go)
