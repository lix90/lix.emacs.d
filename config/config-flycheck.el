;;; flycheck --- flycheck mode configuration
;;; Commentary:
;;; code:
(use-package flycheck
  :ensure t
  :diminish flycheck-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(setq-default flycheck-temp-prefix ".flycheck")
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers '(json-jsonlist)))

(provide 'config-flycheck)
;;; config-flycheck.el ends here
