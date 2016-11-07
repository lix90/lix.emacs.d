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
(defun toggle-transparency ()
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
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; theme
;; (load-theme 'misterioso)
;;----------- from markauskas, my-colors.el
(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai t))
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
(moe-theme-set-color 'cyan)
;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
;; (moe-dark)
(powerline-moe-theme)
(show-paren-mode t)
(setq show-paren-style 'expression)

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

;; automatically resize buffer
;; (use-package golden-ratio
;;   :ensure t
;;   :diminish golden-ratio-mode
;;   :init (progn (golden-ratio-mode t)
;;                (setq golden-ratio-adjust-factor .8
;;                      golden-ratio-wide-adjust-factor .8
;;                      golden-ratio-exclude-modes '(list "projectile-mode" "project-explorer-mode")
;;                      )
;;                (golden-ratio-toggle-widescreen)))

;; from: https://github.com/Henry/dot-emacs/blob/8cadffbc4d077aa64d08e2e1956994f9203de696/init/init-doremi.el
;;; init-doremi.el --- Initialize DoReMi
;;;  Incremental change using arrow keys or mouse wheel
;; -----------------------------------------------------------------------------
(use-package doremi
  :ensure t
  :config
  (use-package doremi-frm :ensure t)  ;; Incrementally adjust frame properties
  (use-package doremi-cmd :ensure t)  ;; Other Do Re Mi commands

  ;; (defvar my-doremi-map (make-sparse-keymap "Do Re Mi"))
  ;; (define-key my-map "d" my-doremi-map)
  ;; (define-key my-doremi-map "b" 'doremi-buffers)
  ;; (define-key my-doremi-map "g" 'doremi-global-marks)
  ;; (define-key my-doremi-map "m" 'doremi-marks)
  ;; (define-key my-doremi-map "r" 'doremi-bookmarks)
  ;; (define-key my-doremi-map "f" 'doremi-frame-width) ;; Frame resize
  ;; (define-key my-doremi-map "w" 'doremi-window-width) ;; Window resize
  ;; (define-key my-doremi-map "p" 'doremi-frame-horizontally)
  ;; (define-key my-doremi-map [return] 'my-doremi-menu)
  ;; (define-key my-doremi-map [mouse-3] 'my-doremi-menu)
  ;; (define-key my-doremi-map [C-tab] 'icicle-complete-keys)
  ) ;; Show options

;; (defun my-doremi-menu ()
;;   (interactive)
;;   (popup-menu
;;    '("Do Re Mi"
;;      ["Buffers" doremi-buffers]
;;      ["Resize Window" doremi-window-width]
;;      ["Resize Frame" doremi-frame-width]
;;      ["Move Frame" doremi-frame-horizontally]
;;      ["Global Marks" doremi-global-marks]
;;      ["Marks in Buffer" doremi-marks]
;;      ["Bookmarks" doremi-bookmarks]
;;      )))


;;-------------------------------------------------------
(provide 'config-appearance)
;;; config-appearance.el ends here
