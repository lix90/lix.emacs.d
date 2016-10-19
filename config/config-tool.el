;;; Google Search In Emacs
(use-package google-this
  :ensure t
  :diminish google-this-mode
  :config
  (google-this-mode 1))

;;; Git in Emacs
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (progn (use-package evil-magit :ensure t)
         ))

(use-package magit-gitflow
  :ensure t
  :after magit
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; (use-package magithub
;;   :ensure t
;;   :after magit)

(use-package weibo
  :ensure t
  :config
  (setq weibo-consumer-key "214135744"
        weibo-consumer-secret "1e0487b02bae1e0df794ebb665d12cf6"
        ))

(provide 'config-tool)
