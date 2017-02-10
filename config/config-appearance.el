;; looking ---looking configuration:
;;; Commentary:
;;; Code:

(use-package better-defaults :ensure t :defer t)

;;
;; Font
;;

(add-hook 'after-init-hook
          (lambda ()
            (fringe-mode '(8 . 2)) ; Make fringe look good with git-gutter-fringe+
            (set-face-attribute
             'default nil
             :family "Source Code Pro"
             :height 120
             :weight 'normal
             :width 'normal)
            ))

;; Highlight current line number
(use-package hlinum :ensure t :defer t :disabled t
  :commands hlinum-mode
  :init
  (add-hook 'linum-mode-hook 'hlinum-activate)
  (add-hook 'prog-mode-hook 'linum-mode))

(add-hook 'package-menu-mode-hook 'hl-line-mode)
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)

(use-package linum-hl-cl-number :load-path "elisp"
  :init (setq linum-format 'linum-highlight-current-line))

(use-package highlight-numbers :ensure t :defer t :disabled t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-todo :ensure t
  :init (global-hl-todo-mode t)
  :config
  (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
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
        '(prog-mode markdown-mode))
  ;;(global-hl-todo-mode 1)
  )

;;------------------------------------------------------------------------------
;; UI
;;------------------------------------------------------------------------------

(use-package fancy-battery :ensure t :after spaceline
  :defer 10 :config (fancy-battery-mode))
(use-package powerline :ensure t :if window-system
  :config (setq-default powerline-default-separator 'nil))
(use-package spaceline :ensure t
  :config (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))
(use-package spaceline-custom :after spaceline :load-path "config")
(use-package spaceline-colors :after spaceline-custom  :load-path "config"
  :init (add-hook 'after-init-hook 'spaceline-update-faces)
  :config (advice-add 'load-theme :after 'spaceline-update-faces))

;; Themed with Spaceline
(use-package gruvbox-theme :ensure t :defer t)
(use-package creamsody-theme :ensure t)
(use-package suscolors-theme :ensure t :defer t)
(use-package atom-one-dark-theme :ensure t :defer t)
(use-package forest-blue-theme :ensure t :defer t)
(use-package liso-theme :ensure t :defer t)
(use-package peacock-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package darktooth-theme :ensure t :defer t)
(use-package spacemacs-theme :ensure t :defer t)

(defun remove-mode-line-box (&rest args)
  (set-face-attribute 'mode-line nil :box nil :underline nil)
  (set-face-attribute 'mode-line-inactive nil :box nil :underline nil))

(when is-mac
  (load-theme 'atom-one-dark t))

(if (not (display-graphic-p))
    (load-theme 'gruvbox t))

;; (when window-system
;;   (remove-mode-line-box)
;;   (load-theme 'spacemacs-light))

(use-package cyphejor :ensure t :defer t
  :init 
  (setq
   cyphejor-rules
   '(:upcase
     ("bookmark" "→")
     ("buffer" "β")
     ("diff" "Δ")
     ("dired" "δ")
     ("emacs" "ε")
     ("Emacs" "ε")
     ("fundamental" "Ⓕ")
     ("inferior" "i" :prefix)
     ("interaction" "i" :prefix)
     ("interactive" "i" :prefix)
     ("lisp" "λ" :postfix)
     ("menu" "▤" :postfix)
     ("mode" "" :postfix)
     ("package" "↓")
     ("python" "π")
     ("shell" "sh" :postfix)
     ("text" "ξ")
     ("wdired" "↯δ")
     ("ess" "𝓔") 
     ("markdown" "𝓜")
     ))
  (cyphejor-mode 1)
  )

(use-package git-gutter+ :ensure t :diminish (git-gutter+-mode)
  :init
  (add-hook 'markdown-mode-hook #'git-gutter+-mode)
  (add-hook 'prog-mode-hook #'git-gutter+-mode)
  :config
  (set-face-foreground 'git-gutter+-added    "royal blue")
  (set-face-foreground 'git-gutter+-modified "orange")
  (set-face-foreground 'git-gutter+-deleted  "hot pink")
  )

(use-package git-gutter-fringe+ :ensure t :after git-gutter+
  :if (display-graphic-p)
  :commands (git-gutter+-mode)
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
    ;;[0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248] 
    ;;[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    [248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248]
    nil nil 'center)
  )

(provide 'config-appearance)
;;; config-appearance.el ends here
