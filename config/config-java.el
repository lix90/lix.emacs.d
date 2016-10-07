
;;; java configuration

(use-package eclim
  :ensure t
  :defer t
  :diminish eclim-mode
  :init (add-hook 'java-mode-hook 'eclim-mode)
  :config
  (progn
    (setq help-at-pt-display-when-idle t
          help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)
    (use-package company-emacs-eclim
      :ensure t
      :defer t
      :config
      (company-emacs-eclim-setup)
      )))

(provide 'config-java)
;;; config-java.el ends here
