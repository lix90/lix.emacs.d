;;; Config-appearance configuration file:
;;; Commentary:
;;; Code:

;; Highlight current line number
;; (use-package hlinum :ensure t :defer t :disabled t
;;   :commands hlinum-mode
;;   :init
;;   (add-hook 'linum-mode-hook 'hlinum-activate)
;;   (add-hook 'prog-mode-hook 'linum-mode))

(require 'ansi-color)

(add-hook 'package-menu-mode-hook #'hl-line-mode)
(add-hook 'buffer-menu-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Highlight current line number
(use-package linum-hl-cl-number :load-path "elisp"
  :init (setq linum-format 'linum-highlight-current-line))

;; (use-package highlight-numbers :ensure t :defer t
;;   :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-todo :ensure t :defer 10
  :config 
  (setq hl-todo-keyword-faces
        '(("HOLD" . "#d0bf8f")
          ("TODO" . "#cc9393")
          ("NEXT" . "#dca3a3")
          ("THEM" . "#dc8cc3")
          ("PROG" . "#7cb8bb")
          ("OKAY" . "#7cb8bb")
          ("DONT" . "#5f7f5f")
          ("FAIL" . "#8c5353")
          ("DONE" . "#afd8af")
          ("FIXME" . "#cc9393")
          ("XXX"   . "#cc9393")
          ("XXXX"  . "#cc9393")
          ("???"   . "#cc9393")))
;;; global-hl-todo-modeで有効にするメジャーモード(derived-mode) 
  (setq hl-todo-activate-in-modes
        '(prog-mode markdown-mode)))


(use-package fancy-battery :ensure t :after spaceline :disabled t
  :defer 10 :config (fancy-battery-mode))
(use-package powerline :ensure t :if window-system :disabled t
  :config (setq-default powerline-default-separator 'nil))
(use-package spaceline :ensure t :disabled t
  :config (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))
(use-package spaceline-custom :after spaceline :load-path "elisp" :disabled t)
(use-package spaceline-colors :after spaceline-custom  :load-path "elisp" :disabled t
  :init (add-hook 'after-init-hook 'spaceline-update-faces)
  :config (advice-add 'load-theme :after 'spaceline-update-faces))


(use-package all-the-icons-dired :ensure t :defer t
  :init (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))
(use-package atom-one-dark-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package twilight-bright-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)
(use-package spacemacs-common :ensure spacemacs-theme :defer t) 
(use-package doom-themes :ensure t
  :config
  (setq doom-enable-bold t
        doom-enable-italic t
        ;; doom-one specific settings
        doom-one-brighter-modeline t
        doom-one-brighter-comments nil)
  ;; brighter source buffers 
  ;;(remove-hook 'find-file-hook 'doom-buffer-mode)
  ;; brighter minibuffer when active
  ;; (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  ;;(remove-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  )


;;; my mode-line-format
(use-package lix-mode-line :load-path "elisp")
(require 'doom-neotree)
(advice-add 'load-theme :after 'lix/update-faces)
(load-theme 'spacemacs-dark t)

(if (not (display-graphic-p))
    (load-theme 'gruvbox t))

(use-package git-gutter+ :ensure t :defer t 
  :config
  (add-hook 'prog-mode-hook #'git-gutter+-mode)
  (set-face-foreground 'git-gutter+-added    "royal blue")
  (set-face-foreground 'git-gutter+-modified "orange")
  (set-face-foreground 'git-gutter+-deleted  "hot pink")
  )

(use-package git-gutter-fringe+ :ensure t :after git-gutter+
  :if (display-graphic-p) 
  :init
  (require 'git-gutter-fringe+)
  (setq git-gutter-fr+-side 'right-fringe) 
  (define-fringe-bitmap 'git-gutter-fr+-added
    ;;[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    [248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr+-modified
    ;;[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    [248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr+-deleted 
    ;;[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    [248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248]
    nil nil 'center))

(provide 'config-appearance)
;;; config-appearance.el ends here
