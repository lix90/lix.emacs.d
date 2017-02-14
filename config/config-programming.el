;; -----------------------------------
;; Programming utilities
;; -----------------------------------

;;; Code:
(use-package smartparens-config :ensure smartparens
  :diminish smartparens-mode
  :init
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (sp-use-smartparens-bindings))

(use-package rainbow-delimiters :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package company :ensure t :defer t
  :init
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-require-match nil
          company-tooltip-limit 10
          company-tooltip-align-annotations t
          company-begin-commands '(self-insert-command))
    ;; set backends
    (setq company-backends
          '(company-elisp
            company-abbrev
            company-keywords
            company-semantic
            ;; company-etags
            company-files
            company-yasnippet)))
  :config
  (progn
    ;; latex
    ;; (add-to-list 'company-backends #'company-latex-commands)
    (use-package company-statistics :ensure t :defer t
      :init (add-hook 'after-init-hook 'company-statistics-mode))

    ;; key bindings
    (let ((map company-active-map))
      (define-key map (kbd "C-/") 'company-search-candidates)
      (define-key map (kbd "C-M-/") 'company-filter-candidates)
      (define-key map (kbd "C-d") 'company-show-doc-buffer)
      (define-key map (kbd "C-j") 'company-select-next)
      (define-key map (kbd "C-k") 'company-select-previous)
      (define-key map (kbd "C-l") 'company-complete-selection))
    ;; Nicer looking faces
    (custom-set-faces
     '(company-tooltip-common
       ((t (:inherit company-tooltip :weight bold :underline nil))))
     '(company-tooltip-common-selection
       ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))))

;; (use-package company-quickhelp
;;   :ensure t
;;   :config
;;   (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))
;; (use-package company-tern
;;   :ensure t
;;   :config (progn
;;             (add-to-list 'company-backends 'company-tern)
;;             (setq company-tern-meta-as-single-line t)
;;             ))

(use-package yasnippet :ensure t :defer t
  :config
  ;; (yas-reload-all)
  (yas-global-mode t)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

;; (use-package nameless :ensure t :defer t
;;   :config
;;   (bind-keys :map nameless-mode-map ("C-c C-c" . nameless-insert-name)))

(use-package flycheck :ensure t
  :config
  ;;(global-flycheck-mode)
  (setq flycheck-javascript-standard-executable "standard")
  (setq flycheck-javascript-eslint-executable "eslint")
  (setq flycheck-eslintrc ".eslintrc.json")
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (bind-keys :map flycheck-mode-map
             ("C-c C-e" . flycheck-list-errors)
             ("C-c C-n" . flycheck-next-error)
             ("C-c C-p" . flycheck-previous-error))
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-standard 'rjsx-mode)
  ;;:bind ("M-}" . flycheck-mode)
  )

(use-package flycheck-pos-tip :ensure t :defer t
  :init (setq-default tooltip-delay 0.2))
(use-package avy-flycheck :ensure t :defer t)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode)
  (avy-flycheck-setup))

(use-package yafolding :ensure t :defer t
  :commands (yafolding-hide-parent-element
             yafolding-toggle-all
             yafolding-toggle-element)
  :init
  (add-hook 'prog-mode-hook 'yafolding-mode))

(use-package indent-tools :ensure t :defer t
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (define-key python-mode-map (kbd "C-c ]") 'indent-tools-hydra/body))))

(provide 'config-programming)
;;; config-programming.el ends here
