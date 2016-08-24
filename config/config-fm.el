;; ---------------------------
;; file management
;; ---------------------------
(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle)
  :config (setq neo-theme 'nerd)
  )


;; setting helm
(use-package helm
  :ensure t
  :init
  (progn
    (require 'helm-config)
    (setq helm-split-window-in-side-p t
          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-move-to-line-cycle-in-source t
          helm-ff-search-library-in-sexp t
          helm-ff-file-name-history-use-recentf t
          helm-echo-input-in-header-line t)
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    ;; (c) Emacs Prelude
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    (helm-mode 1)
    (helm-adaptive-mode 1)
    (helm-push-mark-mode 1))
  :diminish helm-mode
  :bind (("C-c h" . helm-mini)
         ("C-c i" . helm-imenu)
         ("C-h a" . helm-apropos)
         ("C-x f" . helm-recentf)
         ("C-x b" . helm-for-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x))
  )

;; (use-package dired :ensure t)

;; (use-package dired
;;   :ensure t
;;   :config (progn
;;             (use-package dired+ :ensure t)
;;             (use-package dired-details :ensure t)
;;             (use-package dired-details+ :ensure t)
;;             (use-package dired-isearch :ensure t)
;;             (use-package dired-single :ensure t)

;;             (put 'dired-find-alternate-file 'disabled nil)
;;             (setq dired-recursive-copies (quote always))
;;             (setq dired-recursive-deletes (quote top))
;;             (put 'dired-find-alternate-file 'disabled nil)
;;             (setq dired-dwim-target t)

;;             (add-hook 'dired-mode-hook
;;                       (lambda ()
;;                         (define-key dired-mode-map (kbd "<return>")
;;                           'dired-find-alternate-file) ; was dired-advertised-find-file
;;                         (define-key dired-mode-map (kbd "^")
;;                           (lambda () (interactive) (find-alternate-file "..")))))
;;             )
;;   )


(provide 'config-fm)

;;; config-fm.el ends here
