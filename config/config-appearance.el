;;; looking --- looking configuration:
;;; Commentary:
;;; Code:

(use-package better-defaults
  :ensure t
  :defer t)

;; disable startup screen and *scratch* message
(setq inhibit-startup-screen t
      initial-scratch-message nil)
;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq ring-bell-function 'ignore)
(setq cursor-in-non-selected-windows nil
      use-dialog-box nil)

;; stop prompting me, allright?
;; a) y is yes and n is no
(fset 'yes-or-no-p 'y-or-n-p)
;; b) i don't care if the process is running
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; theme
;; (load-theme 'misterioso)
;;----------- from markauskas, my-colors.el
;; (use-package monokai-theme
;;   :ensure t
;;   :init
;;   (load-theme 'monokai t))
;; (use-package color-theme-approximate
;;   :ensure t
;;   :init (progn
;;           (color-theme-approximate-on)))
;; (use-package emojify
;;   :ensure t
;;   :init (add-hook 'after-init-hook #'global-emojify-mode))
;; (use-package powerline
;;   :ensure t
;;   :init
;;   (progn
;;     (powerline-default-theme)
;;     (setq powerline-default-separator 'arrow)
;;     (setq powerline-height 18)
;;     (setq powerline-raw " ")
;;     (setq ns-use-srgb-colorspace nil)))
;; modeline
(use-package spaceline
  :ensure t
  :init (progn
          (require 'spaceline-config)
          (setq powerline-default-separator 'arrow)
          (setq powerline-height 18)
          (setq powerline-raw " ")
          (setq ns-use-srgb-colorspace nil)
          (spaceline-spacemacs-theme)))

(use-package moe-theme
  :ensure t)
;; (set moe-theme-highlight-buffer-id t)
(moe-theme-set-color 'blue)
;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
(moe-dark)
(powerline-moe-theme)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; (use-package rainbow-mode
;; :ensure t)

;; auto-revert
;; (global-auto-revert-mode)
;; (setq global-auto-revert-non-file-buffers t
;;       auto-revert-verbose t)

;; display time
(display-time-mode t)
(setq display-time-24hr-format t)

;; visual line
(global-visual-line-mode t)
(diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)

;;-------------------------------------------------------
(provide 'config-appearance)
;;; config-appearance.el ends here
