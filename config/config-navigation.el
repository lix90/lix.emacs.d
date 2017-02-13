;; General

;;; IMPORTANT
(use-package general :ensure t
  :init
  (general-create-definer leader-key
                          :states '(normal insert visual motion emacs)
                          :keymaps 'global
                          :prefix "SPC"
                          :non-normal-prefix "H-SPC"))

(use-package which-key :ensure t :diminish which-key-mode
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (setq
   which-key-popup-type 'side-window
   which-key-side-window-location 'bottom
   which-key-side-window-max-height 0.30
   which-key-side-window-max-width 0.20
   which-key-max-description-length 25
   which-key-allow-evil-operators t
   which-key-sort-order 'which-key-key-order
   which-key-unicode-correction 3
   which-key-prefix-prefix "+"
   which-key-idle-delay 0.3
   )
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil))))

(use-package avy :ensure t
  :commands (avy-goto-char))

(use-package imenu-anywhere :ensure t :after ivy
  :commands (imenu-anywhere ivy-imenu-anywhere))

;;; swiper, flx, counsel, ivy
(use-package flx :ensure t :defer t)
(use-package swiper :ensure t :defer t :ensure counsel
  :commands (swiper ivy-resume counsel-M-x counsel-find-file)
  :init (ivy-mode 1)
  :config
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-fixed-height-minibuffer t
        ivy-height 20
        ivy-use-virtual-buffers t))

;; take care of the whitespace
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
  :init (smooth-scrolling-mode 1)
  :config
  (setq smooth-scroll-margin 2)
  (setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil))

(use-package centered-cursor-mode :ensure t :defer 10
  :config
  (setq ccm-recenter-at-end-of-file t
        ccm-ignored-commands '(mouse-drag-region
                               mouse-set-point
                               widget-button-click
                               scroll-bar-toolkit-scroll
                               evil-mouse-drag-region)))


;; (use-package ace-jump-mode
;;   :ensure t
;;   :defer t
;;   :commands (ace-jump-word-mode
;;              ace-jump-char-mode
;;              ace-jump-line-mode))

;;; navigate text
(use-package move-text :ensure t :defer t
  :bind
  ("H-n" . move-text-down)
  ("H-p" . move-text-up))

;;; navigate window
(use-package window-numbering :ensure t
  :init (window-numbering-mode 1)
  :config
  (defun window-numbering-install-mode-line (&optional position)
    "Do nothing, the display is handled by the powerline.")
  (setq window-numbering-auto-assign-0-to-minibuffer nil)
  (leader-key
   "0" 'select-window-0
   "1" 'select-window-1
   "2" 'select-window-2
   "3" 'select-window-3
   "4" 'select-window-4
   "5" 'select-window-5)
  (defun spacemacs//window-numbering-assign (windows)
    "Custom number assignment for special buffers."
    (mapc (lambda (w)
            (when (and (boundp 'neo-global--window)
                       (eq w neo-global--window))
              (window-numbering-assign w 0)))
          windows))
  (add-hook 'window-numbering-before-hook 'spacemacs//window-numbering-assign))

(use-package windmove :ensure t :defer t
  :commands (windmove-up
             windmove-down
             windmove-left
             windmove-right)
  :config
  (defun split-window-right-and-focus ()
    "Split the window horizontally and focus the new window."
    (interactive)
    (split-window-right)
    (windmove-right))
  (defun split-window-below-and-focus ()
    "Split the window vertically and focus the new window."
    (interactive)
    (split-window-below)
    (windmove-down)))

(use-package dired-details+ :defer t :ensure dired-details
  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode +1)
              ;; allow dired to delete or copy dir
              (setq dired-recursive-copies 'always) ; “always” means no asking
              (setq dired-recursive-deletes 'top) ; “top” means ask once
              (setq insert-directory-program (executable-find "gls"))
              (setq dired-details-hidden-string " ")
              (setq dired-dwim-target t)
              (define-key dired-mode-map (kbd "RET")
                'dired-find-alternate-file) ; was dired-advertised-find-file
              (define-key dired-mode-map (kbd "^")
                (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
              ))
  (add-hook 'dired-mode-hook 'hl-line-mode)
  :config
  (require 'dired)
  (use-package dired+ :ensure t)
  (use-package dired-single :ensure t)
  )



;;;-----------------------------------------------------------------------------
;;; Project management
;;;-----------------------------------------------------------------------------
;; (use-package project-explorer
;;   :ensure t
;;   :defer t)
;; (add-hook 'project-explorer-mode-hook (lambda ()
;;                                         (unbind-key "M-o" project-explorer-mode-map)))
(use-package neotree
  :ensure t
  :commands (neotree-toggle)
  :config
  (progn
    (setq neo-theme 'nerd
          neo-window-width 30
          neo-window-fixed-size 1)))

;;;-----------------------------------------------------------------------------
(use-package aggressive-indent :ensure t :defer t
  :init
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haml-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package whitespace-cleanup-mode :ensure t :defer t
  :init (global-whitespace-cleanup-mode 1))

(use-package semantic :ensure t :defer t
  :config
  (add-to-list 'semantic-default-submodes
               'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes
               'global-semantic-idle-summary-mode))

(use-package undo-tree :ensure t :defer t
  :commands (undo-tree-undo undo-tree-visualize)
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (let ((undo-dir (concat user-cache-directory "undo")))
    (make-directory undo-dir t)
    (setq undo-tree-history-directory-alist '((".*" . ,undo-dir)))))

;; editing
(use-package expand-region :ensure t :defer t
  :bind ("C-=" . er/expand-region))

(use-package whole-line-or-region :ensure t :defer t
  :diminish whole-line-or-region-mode
  :init
  (whole-line-or-region-mode t))

(use-package multiple-cursors
  :ensure t
  :defer t
  :diminish multiple-cursors-mode
  :config
  (progn
    (use-package phi-search
      :ensure t)
    (define-key mc/keymap (kbd "C-s") 'phi-search)
    (define-key mc/keymap (kbd "C-r") 'phi-search-backward)))

(use-package hungry-delete :ensure t :defer t
  :init (global-hungry-delete-mode 1)
  :diminish hungry-delete-mode)

(use-package fix-word :ensure t :defer t :diminish fix-word
  :bind
  ("H-u" . fix-word-upcase)
  ("H-l" . fix-word-downcase)
  ("H-c" . fix-word-capitalize))

(use-package crux :ensure t
  :bind ("C-a" . crux-move-beginning-of-line))

(use-package phi-rectangle :ensure t :defer t)

(use-package reveal-in-osx-finder
  :ensure t
  :defer t
  :commands (reveal-in-osx-finder))

(provide 'config-navigation)
;;; config-navigation.el ends here
