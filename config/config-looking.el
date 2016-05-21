;; defaults
(use-package better-defaults
  :ensure t
  :defer t)
(require 'better-defaults)

;; theme
(load-theme 'misterioso)

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
 ((font-existsp "Source Code Pro")
  (set-face-attribute 'default nil
                      :height 100
                      :font "Source Code Pro")))
;; (cond
;;  ((eq window-system nil) nil)
;;  ((font-existsp "Source Code Pro")
;;   (set-face-attribute 'default nil
;;                       :height 100
;;                       :font "Source Code Pro")))


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
