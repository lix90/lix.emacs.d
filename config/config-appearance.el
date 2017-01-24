;;; looking --- looking configuration:
;;; Commentary:
;;; Code:
(use-package better-defaults
  :ensure t
  :defer t)

;; disable startup screen and *scratch* message
(setq inhibit-startup-screen t
      initial-scratch-message nil
      ;; nice scrolling
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      cursor-in-non-selected-windows nil
      use-dialog-box nil
      ring-bell-function 'ignore)

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

(use-package smart-mode-line
  :ensure t
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'light
          sml/shorten-directory t
          sml/shorten-modes t)))

(use-package solarized-theme
  :ensure t
  :init
  (progn
    (defun lix--dark-theme()
      (interactive)
      (load-theme 'solarized-dark t))

    (defun lix--light-theme()
      (interactive) 
      (load-theme 'solarized-light t)) 

    (setq solarized-high-contrast-mode-line t
          solarized-distinct-fringe-background t
          solarized-use-variable-pitch nil
          solarized-use-more-italic t)))

(lix--dark-theme)
(sml/setup)

;; (use-package indent-guide
;;   :ensure t
;;   :init
;;   (indent-guide-global-mode)
;;   ;; #fdf6e3 for light theme
;;   (set-face-foreground 'indent-guide-face (face-foreground 'default))
;;   (setq indent-guide-delay 0.1
;;         indent-guide-recursive t
;;         indent-guide-char "|"))

(use-package transpose-frame
  :ensure t
  :defer t
  :bind (("C-c t t" . transpose-frame)
         ("C-c t v" . flip-frame)
         ("C-c t h" . flop-frame)))

(use-package color-theme-approximate
  :ensure t
  :defer t
  :init
  (color-theme-approximate-on))

(use-package rainbow-identifiers
  :ensure t
  :commands (global-rainbow-identifiers-mode
             rainbow-identifiers-mode)
  :init
  (progn
    ;; copied from spacemacs
    ;; (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
    ;;       rainbow-identifiers-cie-l*a*b*-saturation 100
    ;;       rainbow-identifiers-cie-l*a*b*-lightness 40
    ;;       ;; override theme faces
    ;;       rainbow-identifiers-faces-to-override '(highlight-quoted-symbol
    ;;                                               font-lock-keyword-face
    ;;                                               font-lock-function-name-face
    ;;                                               font-lock-variable-name-face))
    (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)))

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

(provide 'config-appearance)
;;; config-appearance.el ends here
