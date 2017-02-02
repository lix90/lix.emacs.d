;;; smartparens --- configuration:
;;; Commentary:
;;; -----------------------------------
;;; Code:

(show-paren-mode t)
;; (setq show-paren-style 'expression)

(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :init
  (progn
    (show-smartparens-global-mode t)
    ;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
    (sp-use-smartparens-bindings)))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide 'config-paren)
;;; config-paren.el ends here
