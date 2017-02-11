;; -----------------------------------
;; Programming utilities
;; -----------------------------------

;;; Code:

(show-paren-mode t)
;; (setq show-paren-style 'expression)

(use-package smartparens-config :ensure smartparens
  :diminish smartparens-mode
  :init
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (sp-use-smartparens-bindings))

(use-package rainbow-delimiters :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package company :ensure t :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.2
		company-minimum-prefix-length 2
		company-require-match nil
		company-tooltip-limit 10
		company-tooltip-align-annotations t
		company-begin-commands '(self-insert-command))
  ;; set backends
  (setq company-backends
		(quote
		 (company-elisp
		  company-abbrev
		  company-keywords
		  company-semantic
		  ;; company-etags
		  company-files
		  company-yasnippet)))
  :config
  ;; latex
  ;; (add-to-list 'company-backends #'company-latex-commands)
  (use-package company-statistics
	:ensure t
	:defer t
	:init
	(add-hook 'after-init-hook 'company-statistics-mode))

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
	 ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

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

;; -----------------------------------
;; yasnippet
;; -----------------------------------
(use-package yasnippet :ensure t :defer t
  :config
  ;; (yas-reload-all)
  (yas-global-mode t)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

;; turn on abbrev mode globally
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)
(diminish 'abbrev-mode)
(setq abbrev-file-name (concat user-cache-directory "emacs_abbre.el"))

;;; sql
(use-package sql
  :commands sql-mode
  :mode (("\.sql$" . sql-mode)
         ("\.sqltmpl$" . sql-mode))
  :config
  (progn
    (use-package sql-indent
      :ensure t
      :config
      (setq sql-indent-offset 2))
    (use-package sqlup-mode
      :ensure t
      :config
      (add-hook 'sql-mode-hook 'sqlup-mode)
      (add-hook 'sql-interactive-mode-hook 'sqlup-mode)))
  )


(provide 'config-programming)
;;; config-programming.el ends here
