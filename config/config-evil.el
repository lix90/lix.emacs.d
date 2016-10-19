;;;

;; Code start:
(use-package evil
  :ensure t
  :config (evil-mode -1))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-key
    "g" 'magit-status))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys)
  )


(provide 'config-evil)
;;; Config-evil.el ends here
