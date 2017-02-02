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




;; visual line
(global-visual-line-mode t)
(diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)

;; Centered Cursor Mode
(use-package centered-cursor-mode
  :defer t
  :diminish centered-cursor-mode
  ;; :commands (centered-cursor-mode
  ;;            global-centered-cursor-mode)
  :config
  (progn
    (setq ccm-recenter-at-end-of-file t
          ccm-ignored-commands '(mouse-drag-region
                                 mouse-set-point
                                 widget-button-click
                                 scroll-bar-toolkit-scroll
                                 evil-mouse-drag-region))))

;; --------------------
;; Diminish
;; --------------------
(use-package diminish :defer 2)
(diminish 'visual-line-mode)
(eval-after-load "company" '(diminish 'company-mode "Ⓒ"))
(eval-after-load "aggressive-indent" '(diminish 'aggressive-indent-mode "Ⓘ"))
(eval-after-load "smartparens" '(diminish 'smartparens-mode "ⓟ"))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode "ⓨ"))
(eval-after-load "ivy" '(diminish 'ivy-mode "ⓘ"))
(eval-after-load "compile" '(diminish 'compilation-shell-minor-mode ">"))
(eval-after-load "flyspell" '(diminish 'flyspell-mode "Ⓢ")) 
(eval-after-load "eldoc" '(diminish 'eldoc-mode "𝓓"))
(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode ""))
(eval-after-load "lispy" '(diminish 'lispy-mode "")) 
(eval-after-load "lispyville" '(diminish 'lispyville-mode "Ⓛ")) 
(eval-after-load "centered-window-mode" '(diminish 'centered-window-mode "⦿"))
(eval-after-load "org-indent" '(diminish 'org-indent-mode))
(eval-after-load "simple" '(diminish 'auto-fill-function "Ⓕ")) 
(eval-after-load "pandoc-mode" '(diminish 'pandoc-mode "Ⓟ"))
(eval-after-load "git-gutter+" '(diminish 'git-gutter+-mode))
(eval-after-load "reftex" '(diminish 'reftex-mode "ⓡ"))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode "Ⓡ"))
(eval-after-load "simple" '(diminish 'auto-revert-mode "Ⓡ"))
(eval-after-load "auto-indent-mode" '(diminish 'auto-indent-mode "ⓘ"))
(eval-after-load "org-zotxt" '(diminish 'org-zotxt-mode ""))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode "Ⓤ"))
(eval-after-load "projectile" '(diminish 'projectile-mode ""))
(eval-after-load "ess-site" '(diminish 'key-combo-mode ""))

(use-package cyphejor
  :ensure t
  :init
  (progn
    (setq
     cyphejor-rules
     '(;;:upcase
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
       ("mode"        "")
       ("package"     "↓")
       ("python"      "π")
       ("shell"       "sh" :postfix)
       ("text"        "ξ")
       ("wdired"      "↯δ")
       ("ess" "𝓔")
       ("Markdown" "𝓜")
       ("markdown" "𝓜")))
    (cyphejor-mode 1))
  )

;; ----------------
;; Font
;; ----------------
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; ----------------
;; Highlight line numbers
;; ----------------
;; line number spacing
(setq linum-format "%4d ")

;; Highlight current line number
(use-package hlinum
  :ensure t
  :commands hlinum-mode
  :init
  (add-hook 'linum-mode-hook 'hlinum-activate)
  (add-hook 'prog-mode-hook 'linum-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-todo
  :ensure t
  :defer t
  :config
  (setq global-hl-todo-mode t))

;; Mode line
(use-package spaceline-config
  :ensure spaceline
  :commands (powerline-reset)
  :defer t
  :init
  (progn
    (powerline-reset))
  :config
  (progn
    (setq powerline-default-separator 'nil)
    (setq powerline-height 18)
    (setq powerline-raw " ")
    (setq ns-use-srgb-colorspace nil)
    ;; fancy git icon for the modeline
    (defadvice vc-mode-line (after strip-backend () activate)
      (when (stringp vc-mode)
        (let ((gitlogo (replace-regexp-in-string "^ Git." ":" vc-mode)))
          (setq vc-mode gitlogo))))
    ;; (require 'spaceline-config)
    (spaceline-toggle-buffer-size-off)
    (spaceline-spacemacs-theme)
    (setq spaceline-buffer-encoding-abbrev-p nil
          spaceline-window-numbers-unicode t
          spaceline-line-column-p nil
          spaceline-buffer-id-p nil
          spaceline-minor-modes-separator nil)

    ))

;; (use-package fancy-battery
;;   :ensure t
;;   :after spaceline
;;   :defer t
;;   :init (fancy-battery-mode)
;;   :config
;;   (setq display-time-format "%a %b %d | %H:%M |")
;;   (display-time-mode))

(setq display-time-format "%a %b %d | %H:%M |")
(display-time-mode)

;; nil - don't use srgb & get proper powerline faces
;; (setq ns-use-srgb-colorspace t)

(use-package smooth-scrolling
  :disabled t
  :defer 2
  :config
  (progn
    (setq smooth-scroll-margin 2)
    (setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
    (setq mouse-wheel-progressive-speed nil))
  :init (smooth-scrolling-mode 1))

(use-package solarized-theme
  :ensure t
  :if (display-graphic-p) 
  :init
  (progn
    ;; don't make the fringe stand out from the background
    (setq solarized-distinct-fringe-background nil)    
    ;; change the font for some headings and titles
    (setq solarized-use-variable-pitch t)
    ;; make the modeline high contrast
    (setq solarized-high-contrast-mode-line t)
    ;; Use bolding
    (setq solarized-use-less-bold nil)
    ;; Use more italics
    (setq solarized-use-more-italic t)
    ;; Use colors for indicators such as git:gutter, flycheck and similar
    (setq solarized-emphasize-indicators t)
    ;; Don't change size of org-mode headlines (but keep other size-changes)
    (setq solarized-scale-org-headlines t) 
    ;; don't italicize line numbers
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (if (display-graphic-p)
                    (set-face-attribute 'linum frame
                                        :background (face-attribute 'default :background)
                                        :foreground (face-attribute 'linum :foreground)
                                        :slant 'normal))))
    (load-theme 'solarized-dark t)
    :config
    ;; Theme toggle
    (setq active-theme 'solarized-dark)
    (defun toggle-dark-light-theme ()
      (interactive)
      (if (eq active-theme 'solarized-light)
          (setq active-theme 'solarized-dark)
        (setq active-theme 'solarized-light))
      (load-theme active-theme)
      (powerline-reset))
    ))

;; Avoid all font-size changes
;; (setq solarized-height-minus-1 1)
;; (setq solarized-height-plus-1 1)
;; (setq solarized-height-plus-2 1)
;; (setq solarized-height-plus-3 1)
;; (setq solarized-height-plus-4 1))


;; An alternative solarized theme
;; (use-package color-theme-sanityinc-solarized
;;   :ensure t
;;   :disabled t
;;   :config
;;   (progn
;;     (load-theme 'sanityinc-solarized-dark t)))

(use-package gruvbox-theme
  :ensure t 
  :if (not (display-graphic-p))
  :config
  (load-theme 'gruvbox t))

(use-package uniquify
  :ensure nil
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package which-key
  :defer 2
  :diminish ""
  :config
  (setq which-key-special-keys nil)
  ;; Set the time delay (in seconds) for the which-key popup to appear.
  (setq which-key-idle-delay .2)
  (which-key-mode))

;; (display-time-mode t)
;; (setq display-time-24hr-format t)
;; (use-package smart-mode-line
;;   :ensure t
;;   :init
;;   (progn
;;     (setq sml/no-confirm-load-theme t)
;;     (setq sml/theme 'light
;;           sml/shorten-directory t
;;           sml/shorten-modes t)))

;; (use-package solarized-theme
;;   :ensure t
;;   :init
;;   (progn
;;     (setq solarized-high-contrast-mode-line t
;;           solarized-distinct-fringe-background t
;;           solarized-use-variable-pitch nil
;;           solarized-use-more-italic t)))

;; (defun lix--light-theme()
;;   (interactive) 
;;   (load-theme 'solarized-light t)
;;   (sml/setup))

;; (defun lix--dark-theme()
;;   (interactive)
;;   (load-theme 'solarized-dark t)
;;   (sml/setup))

;; (lix--dark-theme)

;; (use-package transpose-frame
;;   :ensure t
;;   :defer t)

;; (use-package color-theme-approximate
;;   :ensure t
;;   :defer t
;;   :init
;;   (color-theme-approximate-on))

;; (use-package rainbow-identifiers
;;   :ensure t
;;   :commands (global-rainbow-identifiers-mode
;;              rainbow-identifiers-mode)
;;   :init
;;   (progn
;;     ;; copied from spacemacs
;;     ;; (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
;;     ;;       rainbow-identifiers-cie-l*a*b*-saturation 100
;;     ;;       rainbow-identifiers-cie-l*a*b*-lightness 40
;;     ;;       ;; override theme faces
;;     ;;       rainbow-identifiers-faces-to-override '(highlight-quoted-symbol
;;     ;;                                               font-lock-keyword-face
;;     ;;                                               font-lock-function-name-face
;;     ;;                                               font-lock-variable-name-face))
;;     (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)))

;; (use-package indent-guide
;;   :ensure t
;;   :init
;;   (indent-guide-global-mode)
;;   ;; #fdf6e3 for light theme
;;   (set-face-foreground 'indent-guide-face (face-foreground 'default))
;;   (setq indent-guide-delay 0.1
;;         indent-guide-recursive t
;;         indent-guide-char "|"))

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
