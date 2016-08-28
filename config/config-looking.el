;; defaults
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
;;   (progn
;;     (load-theme 'monokai t)))
(load-theme 'deeper-blue)

(use-package color-theme-approximate
  :ensure t
  :init (progn
          (color-theme-approximate-on)))


;; modeline
;; (use-package spaceline
;;   :ensure t
;;   :init (progn
;;           (require 'spaceline-config)
;;           (setq powerline-default-separator 'arrow)
;;           (setq powerline-height 18)
;;           (setq powerline-raw " ")
;;           (setq ns-use-srgb-colorspace nil)
;;           ;; (setq spaceline-separator-dir-left '(left . left))
;;           ;; (setq spaceline-separator-dir-right '(right . right))
;;           (spaceline-spacemacs-theme)))

;; startup time
;; (use-package esup
;;   :ensure t)

;; auto-revert
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose t)

;; display time
(display-time-mode t)
(setq display-time-24hr-format t)

;; visual line
(global-visual-line-mode t)
(diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)

;;-------------------------------------------------------
(provide 'config-looking)
