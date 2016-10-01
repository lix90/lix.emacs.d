;; -----------------------------------
;; auto completion
;; -----------------------------------

(use-package company
  :ensure t
  :diminish company-mode
  :bind ("C-x M-c" . company-complete)
  :init
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-tooltip-limit 10
          company-tooltip-align-annotations 't
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

(provide 'config-ac)
;;; config-ac.el ends here
