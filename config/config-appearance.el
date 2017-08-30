;;; Config-appearance configuration file:
;;; Commentary:
;;; Code:

(require 'ansi-color)

;; color I like
(defconst hl-color "#ffd446")
(defconst hl-color-dark "#282c34")
;; line number
;;(setq linum-relative-format "%4s ")
(setq linum-format "%4s ")
;; fringe settings
(fringe-mode '(0 . 2))
;; font settings
(setq-default line-spacing 1)
(setq lix/font-height (if is-mac
                          130
                        100))
(set-face-attribute 'default nil
                    :family "Source Code Pro" ; Source Code Pro
                    :height lix/font-height
                    :weight 'normal
                    :width 'normal)

;; UI basic settings
(setq-default menu-bar-mode nil)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when is-mac
  (setq mac-allow-anti-aliasing t))
(setq-default inhibit-startup-screen t
              initial-scratch-message nil
              cursor-in-non-selected-windows nil
              use-dialog-box nil
              ring-bell-function 'ignore
              message-log-max 10000)

;; set cursor face
(setq-default cursor-type 'bar)
(set-face-background 'cursor hl-color)

(dolist (hook '(
                prog-mode-hook
                web-mode-hook 
                matlab-mode-hook 
                ))
  (add-hook 'hook
            (lambda()
              (visual-line-mode +1)
              (global-visual-line-mode -1) 
              ;; line number
              (linum-mode +1)
              ;; highlight current line
              (hl-line-mode +1)
              (set-face-background 'hl-line hl-color-dark)
              (toggle-truncate-lines +1)
              )))

;; (eval-after-load 'linum-mode
;;   (lambda()
;;     (fringe-mode '(0 . 2))))

(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b01110000
   #b00010000
   #b00010000
   #b00000000])

(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00001000
   #b00001000
   #b00001110
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(use-package all-the-icons-dired :ensure all-the-icons :defer t
  :init
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

;; Highlight current line number
(use-package hlinum :ensure t :defer t
  :init
  (add-hook 'prog-mode-hook #'hlinum-activate)
  :config
  (set-face-attribute 'linum-highlight-face nil
                      :background hl-color-dark
                      :foreground hl-color
                      :weight 'bold))


(use-package atom-one-dark-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package twilight-bright-theme :ensure t :defer t :disabled t)
(use-package monokai-theme :ensure t :defer t)
(use-package spacemacs-common :ensure spacemacs-theme :defer t :disabled t) 

(use-package doom-themes :ensure t :defer t
  :config
  (setq doom-enable-bold t
        doom-enable-italic t
        ;; doom-one specific settings
        doom-one-brighter-modeline t
        doom-one-brighter-comments nil))

(use-package doom-neotree :ensure doom-themes :after neotree
  :config
  (setq doom-neotree-line-spacing 1
        doom-neotree-enable-file-icons t
        doom-neotree-folder-size 0.8))

;;; my mode-line-format
(use-package lix-mode-line :load-path "elisp")

(advice-add 'load-theme :after 'lix/update-faces)
;;(load-theme 'spacemacs-dark t)
(if (display-graphic-p)
    (load-theme 'doom-one t))

(use-package gruvbox-theme :ensure t :defer t)
(if (not (display-graphic-p))
    (load-theme 'spacemacs-dark t))

(provide 'config-appearance)
;;; config-appearance.el ends here
