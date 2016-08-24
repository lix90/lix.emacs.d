;; -----------------------------------
;; auto completion
;; -----------------------------------

(use-package company
  :ensure t
  :diminish company-mode
  :init (progn
          (add-hook 'after-init-hook 'global-company-mode)
          (setq company-idle-delay 0.2
                company-minimum-prefix-length 2
                company-require-match nil
                company-dabbrev-ignore-case nil
                company-dabbrev-downcase nil)
          ;; set backends
          (setq company-backends
                (quote
                 (company-elisp
                  ;; company-emoji
                  company-css
                  company-semantic
                  company-etags
                  company-files)))))

(use-package company-quickhelp
  :ensure t)

(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(use-package helm-company
  :ensure t)

(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

(use-package company-tern
  :ensure company-tern)
(eval-after-load 'company
  '(progn (add-to-list 'company-backends 'company-tern)
          (setq company-tern-meta-as-single-line t)))

;; (use-package company
;;   :ensure t
;;   :config (progn
;;             (setq company-idle-delay 0.2
;;                   company-minimum-prefix-length 2
;;                   company-require-match nil
;;                   company-dabbrev-ignore-case nil
;;                   company-dabbrev-downcase nil
;;                   company-frontends '(company-pseudo-tooltip-frontend))

;;             (defvar-local company-fci-mode-on-p nil)
;;             (defun company-turn-off-fci (&rest ignore)
;;               (when (boundp 'fci-mode)
;;                 (setq company-fci-mode-on-p fci-mode)
;;                 (when fci-mode (fci-mode -1))))

;;             (defun company-maybe-turn-on-fci (&rest ignore)
;;               (when company-fci-mode-on-p (fci-mode 1)))

;;             (add-hook 'company-completion-started-hook 'company-turn-off-fci)
;;             (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
;;             (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
;;             (add-hook 'after-init-hook 'global-company-mode)

;; ;; key bindings
;; (let ((map company-active-map))
;;   (define-key map (kbd "C-/") 'company-search-candidates)
;;   (define-key map (kbd "C-M-/") 'company-filter-candidates)
;;   (define-key map (kbd "C-d") 'company-show-doc-buffer)
;;   (define-key map (kbd "C-j") 'company-select-next)
;;   (define-key map (kbd "C-k") 'company-select-previous)
;;   (define-key map (kbd "C-l") 'company-complete-selection))

;;             (use-package company-emoji
;;               :ensure t
;;               :init (company-emoji-init))
;;             (setq company-idle-delay 0.2)
;;             )
;;   :diminish company-mode)

(provide 'config-ac)

;;; config-ac.el ends here
