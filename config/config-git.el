(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package magit-gitflow
  :ensure t
  :after magit
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; (use-package magithub
;;   :ensure t
;;   :after magit)


(provide 'config-git)
