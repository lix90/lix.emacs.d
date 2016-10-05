;; -----------------------------------
;; auto completion
;; -----------------------------------

;;; code:
(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :bind ("C-x M-c" . company-complete)
  :init
  (progn
    (use-package company-emoji :ensure t :defer t)
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
            company-emoji
            company-keywords
            ;; company-css
            company-semantic
            company-etags
            company-files
            company-yasnippet)))))

(use-package company-statistics
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'company-statistics-mode))

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
  :diminish yas-minor-mode
  :init (progn
          ;; (yas-reload-all)
          (yas-global-mode t)
          ;; (add-hook 'prog-mode-hook #'yas-minor-mode)
          ))


(provide 'config-ac)
;;; config-ac.el ends here
