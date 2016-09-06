;; ---------------------------
;; file management
;; ---------------------------
(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle)
  :config (progn
            (setq neo-theme 'nerd)
            )
  )

;;; stolen from: https://github.com/marktran/emacs.d
(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :bind ("C-S-m" . ivy-immediate-done)

  :config
  (use-package counsel
    :ensure t)
  (use-package flx :ensure t)
  (use-package swiper
    :ensure t
    :bind (("<f5>" . swiper)))

  (setq ivy-fixed-height-minibuffer t
        ivy-height 20
        ivy-use-virtual-buffers t

        ivy-ignore-buffers `("^\\*alchemist-server\\*"
                             "^\\*alchemist test report\\*"
                             "^\\*Compile-Log\\*"
                             "^\\*Completions\\*"
                             "^\\*Help\\*"
                             "^\\*Messages\\*"
                             "^\\*Warnings\\*"
                             "^\\*eshell"
                             "^\\*magit"
                             "^\\*scratch\\*"
                             "^\\*rspec-compilation\\*"
                             (lambda (name)
                               (save-excursion
                                 (equal major-mode 'dired-mode))))

        ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (local-file-name "cache/savehist"))
(savehist-mode t)

(require 'desktop)
(setq-default desktop-missing-file-warning nil
              desktop-load-locked-desktop t
              desktop-restore-eager 0
              desktop-path `(,(local-file-name "cache"))
              desktop-save t)
(desktop-save-mode t)

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 30)
                (shell-command-history    . 50)
                tags-file-name
                register-alist))
      desktop-locals-to-save nil)
(desktop-read)

;; save recent files
(use-package recentf
  :init
  (recentf-mode t)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (progn
    (setq recentf-save-file (local-file-name "cache/recentf")
          recentf-max-saved-items 100
          recentf-max-menu-items 25)
    ))

;; setting helm
;; (use-package helm
;;   :ensure t
;;   :config
;;   (progn
;;     (require 'helm-config)
;;     (setq helm-split-window-in-side-p t
;;           helm-M-x-fuzzy-match t
;;           helm-buffers-fuzzy-matching t
;;           helm-recentf-fuzzy-match t
;;           helm-move-to-line-cycle-in-source t
;;           helm-ff-search-library-in-sexp t
;;           helm-ff-file-name-history-use-recentf t
;;           helm-echo-input-in-header-line t)
;;     ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;;     ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;;     ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;;     ;; (c) Emacs Prelude
;;     (global-set-key (kbd "C-c h") 'helm-command-prefix)
;;     (global-unset-key (kbd "C-x c"))
;;     (helm-mode 1)
;;     (helm-adaptive-mode 1)
;;     (helm-push-mark-mode 1))
;;   :diminish helm-mode
;;   :bind (("C-c h" . helm-mini)
;;          ("C-c i" . helm-imenu)
;;          ("C-c a" . helm-apropos)
;;          ("C-x f" . helm-recentf)
;;          ("C-x b" . helm-for-files)
;;          ("C-x C-b" . helm-buffers-list)
;;          ("C-x C-f" . helm-find-files)
;;          ("M-y" . helm-show-kill-ring)
;;          ("M-x" . helm-M-x))
;;   )

;; (use-package dired)

;; (use-package dired-subtree
;;   :ensure t
;;   :bind (:map dired-mode-map
;;               ("C-i" . dired-subtree-insert)
;;               ("C-k" . dired-subtree-remove)
;;               ("C-M-s" . dired-subtree-beginning)
;;               ("C-M-e" . dired-subtree-end)))
;; (use-package dired-filter
;;   :ensure t
;;   :config (define-key dired-mode-map (kbd "C-/") dired-filter-map))


(provide 'config-fm)

;;; config-fm.el ends here
