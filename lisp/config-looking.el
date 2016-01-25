;; defaults
(use-package better-defaults
  :ensure t
  :defer t)
(require 'better-defaults)

;; theme
(load-theme 'misterioso)

;; modeline
(use-package spaceline
  :ensure t
  :config (progn
            (require 'spaceline-config)
            (setq powerline-default-separator 'zigzag)
            (spaceline-spacemacs-theme)))

;; font
(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))
(cond
 ((eq window-system nil) nil)
 ((font-existsp "DejaVu Sans Mono")
  (set-face-attribute 'default nil
                      :height 100
                      :font "DejaVu Sans Mono")))

;; startup time
(use-package esup
  :ensure t)

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
