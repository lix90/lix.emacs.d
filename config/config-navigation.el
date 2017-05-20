;; window navigation by numbers
(use-package winum :ensure t :defer t
  :init
  (leader-key "`" 'winum-select-window-by-number
              "0" 'winum-select-window-0
              "1" 'winum-select-window-1
              "2" 'winum-select-window-2
              "3" 'winum-select-window-3
              "4" 'winum-select-window-4
              "5" 'winum-select-window-5
              "6" 'winum-select-window-6
              "7" 'winum-select-window-7
              "8" 'winum-select-window-8
              "9" 'winum-select-window-9)
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
        winum-ignored-buffers '("*which-key*"))
  (winum-mode t))

;; navigate windows & delete windows
(use-package windmove :ensure t :defer t
  :init
  (windmove-default-keybindings)
  (defun delete-window-below ()
    "Delete window below. (require 'windmove)"
    (interactive)
    (windmove-down)
    (kill-this-buffer)
    (delete-window))
  (defun delete-window-above ()
    "Delete window above. (require 'windmove)"
    (interactive)
    (windmove-up)
    (kill-this-buffer)
    (delete-window))
  (defun delete-window-left ()
    (interactive)
    (windmove-left)
    (kill-this-buffer)
    (delete-windwo))
  (defun delete-window-right ()
    (interactive)
    (windmove-right)
    (kill-this-buffer)
    (delete-window)))

(use-package imenu-anywhere :ensure t :after ivy
  :bind ("M-s i" . ivy-imenu-anywhere))

(use-package flx :ensure t :defer t)

(use-package swiper :ensure t :defer t :ensure counsel
  :commands (ivy-resume counsel-M-x counsel-find-file)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . counsel-grep-or-swiper)
         ("C-c i" . ivy-immediate-done)
         ("M-s p" . counsel-ag))
  :init (add-hook 'after-init-hook #'ivy-mode)
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

(use-package smooth-scrolling :ensure t :defer t
  :init (add-hook 'prog-mode-hook #'smooth-scrolling-mode)
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

(use-package indent-tools :ensure t :defer t :disabled t
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (define-key python-mode-map (kbd "C-c ]") 'indent-tools-hydra/body))))

(use-package semantic :ensure t :defer t :disabled t
  :config
  (add-to-list 'semantic-default-submodes
               '(global-semantic-stickyfunc-mode
                 global-semantic-idle-summary-mode)))

(use-package yafolding :ensure t :defer t :disabled t
  :commands (yafolding-hide-parent-element
             yafolding-toggle-all
             yafolding-toggle-element)
  :config
  (setq yafolding-show-fringe-marks nil)
  (add-hook 'prog-mode-hook #'yafolding-mode))

(use-package popwin :ensure t :defer t :disabled t
  :bind (("C-x m" . popwin:messages))
  :init
  (progn
    (require 'popwin)
    (popwin-mode t))
  :config
  (progn
    (setq popwin:close-popup-window-timer-interval 0.1
          popwin:close-popup-window-timer nil)
    ;; don't use default value but manage it ourselves
    (setq popwin:special-display-config nil)

    (defun spacemacs/remove-popwin-display-config (str)
      "Removes the popwin display configurations that matches the passed STR"
      (setq popwin:special-display-config
            (-remove (lambda (x) (if (and (listp x) (stringp (car x)))
                                     (string-match str (car x))))
                     popwin:special-display-config)))

    ;; buffers that we manage
    (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
    (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
    (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*undo-tree*"            :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)
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
                (define-key flycheck-mode-map (kbd "C-x e") 'popwin:flycheck-errors)))))

(provide 'config-navigation)
;;; config-navigation.el ends here
