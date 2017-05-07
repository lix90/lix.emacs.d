;; php mode
(use-package php-mode :ensure t :disabled t
  :mode ("\\.php\\'" . php-mode)
  :config
  (progn
    (use-package ac-php
      :ensure t
      :after php-mode)
    (use-package php-eldoc
      :ensure t
      :after php-mode)
    ))

;; (use-package company-php
;;   :ensure t
;;   :after php-mode)

(eval-after-load 'php-mode
  '(progn
     (add-hook 'php-mode-hook 'smartparens-mode)
     (add-hook 'php-mode-hook 'php-eldoc-enable)
     (add-hook 'php-mode-hook
               (lambda ()
                 (company-mode t)
                 (add-to-list 'company-backends 'company-ac-php-backend)))))

(use-package psysh :disabled t
  :if (executable-find "psysh")
  :ensure t)

(provide 'config-php)
