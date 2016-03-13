(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (progn (use-package magit-gitflow
           :ensure t
           :init
           (progn (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))))


(provide 'config-tool)
