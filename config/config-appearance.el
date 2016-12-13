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
;; transparency
(defun lix--toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(70 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c u t") 'lix--toggle-transparency)

;; paren
(show-paren-mode t)
(setq show-paren-style 'expression)
;; display time
(display-time-mode t)
(setq display-time-24hr-format t)
;; visual line
(global-visual-line-mode t)
(diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)

(use-package solarized-theme
  :ensure t
  :init
  (defun lix--solarized-config ()
    (setq solarized-high-contrast-mode-line t
          solarized-distinct-fringe-background t
          solarized-use-variable-pitch nil
          solarized-use-more-italic t))
  ;; load theme
  (load-theme 'solarized-dark t))

(use-package smart-mode-line
  :ensure t
  ;;:init
  ;; (defun lix--sml-config ()
  ;;   (sml/setup t)
  ;;   (setq sml/theme 'light
  ;;         sml/shorten-directory t
  ;;         sml/shorten-modes t))
  )

(defun lix--dark-theme ()
  "Change to dark theme."
  (interactive)
  (load-theme 'solarized-dark t)
  (lix--solarized-config)
  ;;(lix--sml-config)
  (indent-guide-global-mode)
  (set-face-background 'indent-guide-face "#002b36"))

(defun lix--light-theme ()
  "Change to light theme."
  (interactive)
  (load-theme 'solarized-light t)
  (lix--solarized-config)
  ;;(lix--sml-config)
  (indent-guide-global-mode)
  (set-face-background 'indent-guide-face "#fdf6e3"))

(lix--dark-theme)
(sml/setup)

(use-package indent-guide
  :ensure t
  :init
  (indent-guide-global-mode)
  ;; #fdf6e3 for light theme
  (set-face-foreground 'indent-guide-face (face-foreground 'default))
  (setq indent-guide-delay 0.1
        indent-guide-recursive t
        indent-guide-char "|"))

;; (use-package monokai-theme
;;   :ensure t
;;   :init
;;   (load-theme 'monokai t))



;; (use-package golden-ratio
;;   :ensure t
;;   :diminish golden-ratio-mode
;;   :init
;;   (golden-ratio-mode t)
;;   (setq golden-ratio-adjust-factor .8
;;         golden-ratio-wide-adjust-factor .8
;;         golden-ratio-exclude-modes '("projectile-mode" "project-explorer-mode"))
;;   (golden-ratio-toggle-widescreen))

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
;; (use-package spaceline
;;   :ensure t
;;   :init (progn
;;           (require 'spaceline-config)
;;           (setq powerline-default-separator 'arrow)
;;           (setq powerline-height 18)
;;           (setq powerline-raw " ")
;;           (setq ns-use-srgb-colorspace nil)
;;           (spaceline-spacemacs-theme)))
;; (use-package moe-theme
;;   :ensure t
;;   :init
;;   (progn (moe-theme-set-color 'red)
;;          (powerline-moe-theme)))
;; (set moe-theme-highlight-buffer-id t)
;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
;; (moe-dark)
;; auto-revert
;; (global-auto-revert-mode)
;; (setq global-auto-revert-non-file-buffers t
;;       auto-revert-verbose t)

;;-------------------------------------------------------
(provide 'config-appearance)
;;; config-appearance.el ends here
