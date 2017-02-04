;; -----------------------------------
;; Programming utilities
;; -----------------------------------

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


(use-package company
  :ensure t
  :defer t
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
          (quote
           (company-elisp
            company-abbrev
            company-keywords
            company-semantic
            ;; company-etags
            company-files
            company-yasnippet))))
  :config
  (progn
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
  )

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
(use-package yasnippet
  :ensure t
  :defer t
  :config
  (progn
    ;; (yas-reload-all)
    (yas-global-mode t)
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    ))

;; turn on abbrev mode globally
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)
(diminish 'abbrev-mode)
;; (setq abbrev-file-name "~/emacs_abbre.el")

;;------------------------------------------------------------------------------
;; parenthesis
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

;;; flycheck

(use-package flycheck
  :ensure t
  :defer t
  :config
  (progn
    (use-package flycheck-pos-tip
      :ensure t
      :defer t
      :init
      (flycheck-pos-tip-mode))

    (setq flycheck-standard-error-navigation nil
          flycheck-global-modes nil)

    ;; Custom fringe indicator
    (when (and (fboundp 'define-fringe-bitmap)
               (not syntax-checking-use-original-bitmaps))
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000)))

    (let ((bitmap (if syntax-checking-use-original-bitmaps
                      'flycheck-fringe-bitmap-double-arrow
                    'my-flycheck-fringe-indicator)))

      (flycheck-define-error-level 'error
        :severity 2
        :overlay-category 'flycheck-error-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-error)
      (flycheck-define-error-level 'warning
        :severity 1
        :overlay-category 'flycheck-warning-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-warning)
      (flycheck-define-error-level 'info
        :severity 0
        :overlay-category 'flycheck-info-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-info))
    ;; copied from spacemacs
    ;; toggle flycheck window
    (defun lix--toggle-flycheck-error-list ()
      "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
      (interactive)
      (-if-let (window (flycheck-get-error-list-window))
          (quit-window nil window)
        (flycheck-list-errors)))

    (defun lix--goto-flycheck-error-list ()
      "Open and go to the error list buffer."
      (interactive)
      (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
        (flycheck-list-errors)
        (switch-to-buffer-other-window flycheck-error-list-buffer)))

    (evilified-state-evilify-map flycheck-error-list-mode-map
                                 :mode flycheck-error-list-mode
                                 :bindings
                                 "RET" 'flycheck-error-list-goto-error
                                 "j" 'flycheck-error-list-next-error
                                 "k" 'flycheck-error-list-previous-error)

    ;; (add-hook 'after-init-hook #'global-flycheck-mode)
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers '(javascript-jshint)))
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (setq-default flycheck-temp-prefix ".flycheck")
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers '(json-jsonlist)))
    )
  )

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
