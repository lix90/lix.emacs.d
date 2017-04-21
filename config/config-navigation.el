;;; Better editor
(use-package general :ensure t :defer t
  :init
  (general-create-definer leader-key
                          ;;                          :states '(normal insert visual motion emacs)
                          :keymaps 'global
                          :prefix "H-SPC"
                          ;;:non-normal-prefix "H-SPC"
                          ))

(use-package which-key :ensure t
  :init
  (which-key-setup-minibuffer)
  (which-key-mode)
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.30
        which-key-side-window-max-width 0.20
        which-key-max-description-length 25
        which-key-allow-evil-operators t
        which-key-sort-order 'which-key-key-order
        which-key-unicode-correction 3
        which-key-prefix-prefix "+"
        which-key-idle-delay 0.3))

(use-package popwin :ensure t
  :bind (("C-x m" . popwin:messages))
  :init
  (require 'popwin)
  (popwin-mode t)
  :config
  (setq popwin:close-popup-window-timer-interval 0.1
        popwin:close-popup-window-timer nil)
  (defun popwin:flycheck-errors ()
    (interactive)
    (when (get-buffer "*Flycheck errors*") (popwin:popup-buffer "*Flycheck errors*")))
  (defun popwin:compilation ()
    (interactive)
    (when (get-buffer "*compilation*")
      (if (get-buffer-window "*compilation*")
          (delete-window (get-buffer-window "*compilation*"))
        (popwin:popup-buffer "*compilation*" :noselect t :stick t :tail t))))
  (add-hook 'flycheck-mode-hook
            (lambda()
              (define-key flycheck-mode-map (kbd "C-x e") 'popwin:flycheck-errors))))

(use-package winum :ensure t :defer t
             :config
             (defun spacemacs//winum-assign-func ()
               "Custom number assignment for neotree."
               (when (and (boundp 'neo-buffer-name)
                          (string= (buffer-name) neo-buffer-name)
                          ;; in case there are two neotree windows. Example: when
                          ;; invoking a transient state from neotree window, the new
                          ;; window will show neotree briefly before displaying the TS,
                          ;; causing an error message. the error is eliminated by
                          ;; assigning 0 only to the top-left window
                          (eq (selected-window) (frame-first-window)))
                 0))
             (setq winum-auto-assign-0-to-minibuffer nil
                   winum-assign-func 'spacemacs//winum-assign-func
                   winum-auto-setup-mode-line nil
                   winum-ignored-buffers '("*which-key*"
                                           "*project-explorer*"))
             (winum-mode t))

(use-package windmove :ensure t :defer t
  :bind (("S-<left>" . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<down>" . windmove-down)
         ("S-<up>" . windmove-up)))

(use-package goto-last-change :ensure t
  :bind (("C-x <" . goto-last-change)))

;;; Directory mangament
(use-package dired+ :ensure t :defer t)
(use-package dired-single :ensure t :defer t)
(use-package dired-details+ :defer t :ensure dired-details)
(add-hook 'dired-mode-hook
          (lambda ()
            (put 'dired-find-alternate-file 'disabled nil)
            (dired-hide-details-mode t)
            (hl-line-mode t)
            ;; allow dired to delete or copy dir
            (setq dired-details-hidden-string " ")
            (setq dired-recursive-copies 'always) ; “always” means no asking
            (setq dired-recursive-deletes 'top) ; “top” means ask once
            (setq insert-directory-program (executable-find "gls"))
            (setq dired-dwim-target t)
            (define-key dired-mode-map (kbd "RET")
              'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))
(add-hook 'dired-load-hook
          (lambda()
            (load "dired-x")))

(use-package project-explorer :ensure t :defer t
             :commands (project-explorer-toggle)
             :init
             (setq pe/width 25))

(use-package neotree :ensure t
  :commands (neotree-toggle)
  :bind ("<f7>" . neotree-toggle)
  :config
  (require 'neotree)
  (setq-default neo-keymap-style 'concise)
  ;;(setq-default neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-theme 'icon)
  (setq  neo-show-hidden-files t
         neo-modern-sidebar nil
         neo-dont-be-alone t
         neo-banner-message nil
         neo-create-file-auto-open t
         neo-window-width 20
         neo-window-fixed-size t
         neo-smart-open t
         neo-auto-indent-point t
         neo-confirm-delete-directory-recursively t
         neo-confirm-delete-file t
         neo-vc-integration 'face
         neo-mode-line-type 'neotree)
  ;; Solved this problem
  ;; https://github.com/jaypei/emacs-neotree/issues/50
  (setq-default neo-persist-show t)
  (when neo-persist-show
    (add-hook 'popwin:before-popup-hook
              (lambda () (setq neo-persist-show nil)))
    (add-hook 'popwin:after-popup-hook
              (lambda () (setq neo-persist-show t))))
             ;;; Code retrieved from https://github.com/jaypei/emacs-neotree/issues/218
  (defun lix/text-scale-twice ()
    "Text scale for neotree."
    (interactive)
    (progn
      (text-scale-adjust 0)
      (text-scale-decrease 1)))
  (add-hook 'neo-after-create-hook
            (lambda (_)
              (call-interactively 'lix/text-scale-twice)
              (visual-line-mode t)
              (setq truncate-lines t)))
             ;;; I want to hind neotree when files open
  ;; retrieved from https://github.com/jaypei/emacs-neotree/issues/77
  )

(use-package projectile :ensure t :defer t
  :init (projectile-global-mode)
  :config (setq projectile-switch-project-action 'neotree-projectile-action))
(use-package counsel-projectile :ensure t :defer t
             :init (counsel-projectile-on))

;;; Searching and marking
(use-package iedit :ensure t) ;; search & editing, alternative for multiple-cursor
(use-package phi-search :ensure t :defer t)
(use-package multiple-cursors :ensure t :defer t :disabled t
  :bind (:map mc/keymap
              ("C-s" . phi-search)
              ("C-r" . phi-search-backward)))
(use-package helm-ag :ensure t
  :bind ("M-s p" . helm-do-ag-project-root))
(use-package phi-rectangle :ensure t :defer t
  :bind (("C-x s" . phi-rectangle-set-mark-command)))
(use-package expand-region :ensure t :defer t
  :bind ("C-=" . er/expand-region))
(use-package avy :ensure t :defer t
  :bind (("M-s a l" . avy-goto-line)
         ("M-s a c" . avy-goto-char)
         ("M-s a C" . avy-goto-char-2)
         ("M-s a w" . avy-goto-word-0)
         ("M-s a W" . avy-goto-word-1)))
(use-package reveal-in-osx-finder :ensure t :defer t
  :bind (("C-x f" . reveal-in-osx-finder)))
(use-package imenu-anywhere :ensure t :after ivy
  :bind ("M-s i" . ivy-imenu-anywhere))
(use-package flx :ensure t :defer t)
(use-package hydra :ensure t :defer t)
(use-package swiper :ensure t :defer t :ensure counsel
  :commands (swiper ivy-resume counsel-M-x counsel-find-file)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . swiper))
  :init (ivy-mode t)
  :config
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-fixed-height-minibuffer t
        ivy-height 20
        ivy-use-virtual-buffers t
        ivy-display-style 'fancy)
  ;;advise swiper to recenter on exit
  (defun lix/swiper-recenter (&rest args)
    "recenter display after swiper"
    (recenter))
  (advice-add 'swiper :after #'lix/swiper-recenter))

;;; Editing
(use-package undo-tree :ensure t :defer t
  :commands (undo-tree-undo undo-tree-visualize)
  :init (global-undo-tree-mode t)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (let ((undo-dir (concat user-cache-directory "undo")))
    (make-directory undo-dir t)
    (setq undo-tree-history-directory-alist `((".*" . ,undo-dir)))))

(use-package crux :ensure t
  :bind ("C-a" . crux-move-beginning-of-line))

(use-package fix-word :ensure t :defer t :diminish fix-word
  :bind (("H-u" . fix-word-upcase)
         ("H-l" . fix-word-downcase)
         ("H-c" . fix-word-capitalize)))

(use-package whitespace :ensure t :defer t
  :init
  (setq whitespace-style '(face
                           trailing
                           lines-tail
                           space-before-tab
                           indentation
                           space-after-tab)
        whitespace-line-column 80))

(use-package smooth-scrolling :ensure t :defer 5
  :init (smooth-scrolling-mode t)
  :config
  (setq smooth-scroll-margin 2)
  (setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil))

(use-package centered-cursor-mode :ensure t :defer 10 :disabled t
  :config
  (setq ccm-recenter-at-end-of-file t
        ccm-ignored-commands '(mouse-drag-region
                               mouse-set-point
                               widget-button-click
                               scroll-bar-toolkit-scroll
                               evil-mouse-drag-region)))

(use-package move-text :ensure t :defer t
  :bind (("H-n" . move-text-down)
         ("H-p" . move-text-up)))

(use-package indent-tools :ensure t :defer t
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (define-key python-mode-map (kbd "C-c ]") 'indent-tools-hydra/body))))

(use-package aggressive-indent :ensure t :defer t
  :init (global-aggressive-indent-mode t)
  :config
  (add-to-list 'aggressive-indent-excluded-modes '(python-mode
                                                   haml-mode
                                                   html-mode)))

(use-package aggressive-fill-paragraph :ensure t :defer t :disabled t
  :config
  (afp-setup-recommended-hooks)
  ;;(add-to-list 'afp-fill-comments-only-mode-list 'python-mode)
  )

(use-package whitespace-cleanup-mode :ensure t :defer t
  :init (global-whitespace-cleanup-mode t))

(use-package semantic :ensure t :defer t
  :config
  (add-to-list 'semantic-default-submodes
               '(global-semantic-stickyfunc-mode
                 global-semantic-idle-summary-mode)))

(use-package yafolding :ensure t :defer t
  :commands (yafolding-hide-parent-element
             yafolding-toggle-all
             yafolding-toggle-element)
  :config
  (setq yafolding-show-fringe-marks nil)
  (add-hook 'prog-mode-hook #'yafolding-mode))

(use-package whole-line-or-region :ensure t :defer t
  :init (whole-line-or-region-mode t))

(use-package hungry-delete :ensure t :defer t
  :init (global-hungry-delete-mode 1)
  :diminish hungry-delete-mode)

(provide 'config-navigation)
;;; config-navigation.el ends here
