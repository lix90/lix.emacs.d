(use-package fancy-battery :ensure t :after spaceline :disabled t
  :defer 10 :config (fancy-battery-mode))
(use-package powerline :ensure t :if window-system :disabled t
  :config (setq-default powerline-default-separator 'nil))
(use-package spaceline :ensure t :disabled t
  :config (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))
(use-package spaceline-custom :after spaceline :load-path "elisp" :disabled t)
(use-package spaceline-colors :after spaceline-custom  :load-path "elisp" :disabled t
  :init (add-hook 'after-init-hook 'spaceline-update-faces)
  :config (advice-add 'load-theme :after 'spaceline-update-faces))

;;; ------------------------------------------------------------------
;;; upgrade-packages
(use-package popup :ensure t :defer t :disabled t
  :config
  ;; Change popup tip face
  ;; TODO make popup tip scrollable
  (set-face-attribute 'popup-tip-face nil
                      :foreground "white"
                      :background "yellow")

  ;; Describe function/variable etc. in popup
  (defun describe-thing-in-popup ()
    (interactive)
    (let* ((thing (symbol-at-point)))
      (cond
       ((fboundp thing) (describe-in-popup 'describe-function))
       ((boundp thing) (describe-in-popup 'describe-variable)))))

  (defun describe-in-popup (fn)
    (let* ((thing (symbol-at-point))
           (description (save-window-excursion
                          (funcall fn thing)
                          (switch-to-buffer "*Help*")
                          (buffer-string))))
      (popup-tip description
                 :point (point)
                 :around t
                 :height 30
                 :scroll-bar t
                 :margin t))))

(use-package dash-at-point :ensure t :defer t :disabled t
  :commands (dash-at-point dash-at-point-with-docset)
  :config
  (add-to-list 'dash-at-point-mode-alist '((perl-mode . "perl")
                                           (ess-mode . "r")
                                           (inferior-ess-mode . "r")
                                           (python-mode . "py2"))))

(use-package counsel-dash :ensure t :defer 30 :disabled t
  :config
  (defun counsel-dash-at-point ()
    "Counsel dash with selected point"
    (interactive)
    (counsel-dash
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (substring-no-properties (or (thing-at-point 'symbol) "")))))
  ;; from YiLiu6240/yxl-spacemacs
  (defun yxl-dash//activate-package-docsets (path)
    "Add dash docsets from specified PATH."
    (setq helm-dash-docsets-path (expand-file-name path))
    (setq helm-dash-common-docsets (helm-dash-installed-docsets))
    (message (format "activated %d docsets from: %s"
                     (length helm-dash-common-docsets) path)))
  (setq counsel-dash-browser-func 'browse-url)
  ;;(yxl-dash//activate-package-docsets "~/.docsets")
  )

(use-package chinese-yasdcv :ensure t :defer t :disabled t
  :commands (yasdcv-translate-at-point)
  :ensure chinese-pyim
  :config
  ;; (setq yasdcv-sdcv-command
  ;;       "sdcv --non-interactive --utf8-output --utf8-input --use-dict \"%dict\" \"%word\"")
  (setq yasdcv-sdcv-dicts
        '(("21century" "21世纪英汉汉英双向词典" "21cen" t)
          ("oxford" "牛津简明英汉袖珍辞典" "21cen" t)
          ("langdce" "朗道汉英字典5.0" "langdao" t)
          ("langdec" "朗道英汉字典5.0" "langdao" t)
          ("webster" "Random House Webster's Unabridged Dictionary (En-En)" "oald" t)
          ("mwcolle" "Merriam-Webster's Collegiate 11th Ed. (En-En)" "oald" t)
          ("mwcolth" "Merriam-Webster's Collegiate Thesaurus (En-En)" "oald" t)
          ("oxfcol" "Oxford Collocations Dictionary 2nd Ed. (En-En)" "21cen" t)
          ("wordnet" "WordNet® 3.0 (En-En)" "21cen" t)
          ("oxfce" "牛津现代英汉双解词典" "oald" nil)
          ("oxfdic1" "Oxford English Dictionary 2nd Ed. P1" "oald" nil)
          ("oxfdic2" "Oxford English Dictionary 2nd Ed. P2" "oald" nil)
          ("amherit" "American Heritage Dictionary 4th Ed. (En-En)" nil nil)
          ("macmillan" "Macmillan English Thesaurus (En-En)" nil nil)
          )))
(use-package github-browse-file :ensure t :defer t :disabled t)
(use-package google-this :ensure t :defer t :disabled t
  :commands (google-this))

;; RSS reader
;; Learning from:
;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/

(use-package elfeed :ensure t :defer t :disabled t
  :commands (elfeed)
  :bind (:map elfeed-search-mode-map
              ("A" . lix/elfeed-show-all)
              ("E" . lix/elfeed-show-emacs)
              ("D" . lix/elfeed-show-daily)
              ("q" . lix/elfeed-save-db-and-bury))
  :init
  (setq url-queue-timeout 30
        elfeed-curl-program-name "curl")
  (setq elfeed-feeds
        '(("https://blog.yitianshijie.net/feed/" chinese blog)
          ))

  ;; elfeed feed reader
  ;; shortcut functions
  (defun lix/elfeed-show-all ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-all"))
  (defun lix/elfeed-show-emacs ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-emacs"))
  (defun lix/elfeed-show-daily ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-daily"))

  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun lix/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun lix/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window)))

;; use an org file to organise feeds
(use-package elfeed-org :ensure t :defer t :disabled t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (concat user-emacs-directory "elfeed.org"))))

(use-package elfeed-goodies :ensure t :defer t :disabled t
  :config (elfeed-goodies/setup))

(use-package el2markdown :ensure t :defer 20 :disabled t)  ;; TODO configuration

(use-package spaceline-config :ensure spaceline :defer t :disabled t
  :commands (powerline-reset)
  :init
  (progn
    (powerline-reset))
  :config
  (progn
    (setq powerline-default-separator 'nil)
    (setq powerline-height 18)
    (setq powerline-raw " ")
    ;; nil - don't use srgb & get proper powerline faces
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

(use-package doom-themes :ensure t :disabled t
  :config
  (load-theme 'doom-one t) ;; or doom-dark, etc.

;;; Settings (defaults)
  (setq doom-enable-bold t    ; if nil, bolding are universally disabled
        doom-enable-italic t  ; if nil, italics are universally disabled
        ;; doom-one specific settings
        doom-one-brighter-modeline nil
        doom-one-brighter-comments nil
        )

  ;; brighter source buffers
  (add-hook 'find-file-hook 'doom-buffer-mode)
  ;; brighter minibuffer when active
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  )

;; (use-package mode-icons
;;   :ensure t
;;   :disabled t
;;   :commands mode-icons-mode
;;   :init
;;   (add-hook 'after-init-hook 'mode-icons-mode))

;; (use-package all-the-icons
;;   :ensure t
;;   :defer t)

;; (use-package all-the-icons-dired
;;   :ensure t
;;   :disabled t
;;   :diminish all-the-icons-dired-mode
;;   :commands all-the-icons-dired-mode
;;   :init
;;   (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; (use-package diminish :defer 2)
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

(use-package solarized-theme :ensure t :if (display-graphic-p)
  :init
  (progn
    ;; don't make the fringe stand out from the background
    (setq solarized-distinct-fringe-background nil)
    ;; change the font for some headings and titles
    (setq solarized-use-variable-pitch t)
    ;; make the modeline high contrast
    (setq solarized-high-contrast-mode-line nil)
    ;; Use bolding
    (setq solarized-use-less-bold t)
    ;; Use more italics
    (setq solarized-use-more-italic t)
    ;; Use colors for indicators such as git:gutter, flycheck and similar
    (setq solarized-emphasize-indicators t)
    ;; Don't change size of org-mode headlines (but keep other size-changes)
    (setq solarized-scale-org-headlines t)

    ;; Underline position setting for X
    (setq x-underline-at-descent-line nil)
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
    (progn
      ;; Theme toggle
      (setq active-theme 'solarized-dark)
      (defun toggle-dark-light-theme ()
        (interactive)
        (if (eq active-theme 'solarized-light)
            (setq active-theme 'solarized-dark)
          (setq active-theme 'solarized-light))
        (load-theme active-theme)
        (powerline-reset))
      )))

;; (use-package fancy-battery
;;   :ensure t
;;   :after spaceline
;;   :defer t
;;   :init (fancy-battery-mode)
;;   :config
;;   (setq display-time-format "%a %b %d | %H:%M |")
;;   (display-time-mode))

;; (use-package smart-mode-line
;;   :ensure t
;;   :init
;;   (progn
;;     (setq sml/no-confirm-load-theme t)
;;     (setq sml/theme 'light
;;           sml/shorten-directory t
;;           sml/shorten-modes t)))

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


(defun custom-modeline-modified
    ((let* ((config-alist
             '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
               ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
               ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
            (result (cdr (assoc (format-mode-line "%*") config-alist))))
       (propertize (apply (cadr result) (cddr result))
                   'face `(:family ,(funcall (car result)))))))

(defun custom-modeline-window-number()
  (propertize (format " %c" (+ 9311 (window-numbering-get-number)))
              'face `(:height ,(/ (* 0.90 powerline/default-height) 100.0)) 'display '(raise 0.0)))

(defun custom-modeline-mode-icon ()
  (format " %s" (propertize icon 'help-echo (format "Major-mode: `%s`" major-mode)
                            'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))

(defun custom-modeline-region-info ()
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
                   'face `(:family ,(all-the-icons-octicon-family))
                   'display '(raise -0.0))
       (propertize (format " (%s, %s)" words chars)
                   'face `(:height 0.9))))))

(defun -custom-modeline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
     " · "
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                 'display '(raise -0.1))
     (propertize (format " %s" branch) 'face `(:height 0.9)))))

(defun -custom-modeline-svn-vc ()
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
     (propertize (format " · %s" revision) 'face `(:height 0.9)))))

(defun custom-modeline-icon-vc ()
  (when vc-mode
    (cond
     ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
     ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
     (t (format "%s" vc-mode)))))

(defun custom-modeline-flycheck-status ()
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                                (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                               (+ (or .warning 0) (or .error 0)))))
                                  (format "✖ %s Issue%s" count (unless (eq 1 count) "s")))
                              "✔ No Issues"))
                 (`running     "⟲ Running")
                 (`no-checker  "⚠ No Checker")
                 (`not-checked "✖ Disabled")
                 (`errored     "⚠ Error")
                 (`interrupted "⛔ Interrupted")
                 (`suspicious  ""))))
    (propertize text
                'help-echo "Show Flycheck Errors"
                'mouse-face '(:box 1)
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

(defvar powerline/upgrades nil)

(defun powerline/count-upgrades ()
  (let ((buf (current-buffer)))
    (package-list-packages-no-fetch)
    (with-current-buffer "*Packages*"
      (setq powerline/upgrades (length (package-menu--find-upgrades))))
    (switch-to-buffer buf)))
(advice-add 'package-menu-execute :after 'powerline/count-upgrades)

(defun custom-modeline-package-updates ()
  (let ((num (or powerline/upgrades (powerline/count-upgrades))))
    (when (> num 0)
      (propertize
       (concat
        (propertize (format "%s" (all-the-icons-octicon "package"))
                    'face `(:family ,(all-the-icons-octicon-family) :height 1.2)
                    'display '(raise -0.1))
        (propertize (format " %d updates " num)
                    'face `(:height 0.9)))
       'help-echo "Open Packages Menu"
       'mouse-face '(:box 1)
       'local-map (make-mode-line-mouse-map
                   'mouse-1 (lambda () (interactive) (package-list-packages)))))))

(defun custom-modeline-time ()
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
    (concat
     (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
     (propertize (format "%s " icon) 'face `(:height 1.0 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))

(setq mode-line-format '("%e" (:eval
                               (concat
                                (custom-modeline-modified)
                                (custom-modeline-window-number)
                                (custom-modeline-mode-icon)
                                (custom-modeline-icon-vc)
                                (custom-modeline-region-info)
                                (custom-modeline-flycheck-status)
                                (custom-modeline-suntime)
                                ;;(custom-modeline-weather)
                                (custom-modeline-time)))))

;; (use-package google-translate
;;   :ensure t
;;   :defer t
;;   :bind (("C-c u d g" . google-translate-at-point)
;;          ("C-c u d G" . google-translate-smooth-translate)
;;          ("C-c u d r" . google-translate-at-point-reverse)
;;          ("C-c u d R" . google-translate-query-translate-reverse))
;;   :config
;;   (progn (require 'google-translate-smooth-ui)
;;          (setq google-translate-default-source-language "en"
;;                google-translate-default-target-language "zh"
;;                max-mini-window-height 0.5)
;;          (setq google-translate-translation-directions-alist
;;                '(("en" . "zh") ("zh" . "zh"))))
;;   )

;; project management
;; (use-package projectile
;;   :ensure t
;;   :init
;;   (projectile-global-mode))

;; (use-package sr-speedbar
;;   :ensure t
;;   :config
;;   (setq sr-speedbar-right-side nil)
;;   (setq sr-speedbar-auto-refresh nil)
;;   (setq sr-speedbar-width 40)
;;   (setq sr-speedbar-max-width 80)
;;   (setq speedbar-smart-directory-expand-flag t)
;;   (setq speedbar-use-images nil)
;;   (speedbar-add-supported-extension ".el")
;;   (add-hook 'speedbar-mode-hook #'(lambda () (visual-line-mode -1))))

;; (use-package emms
;;   :ensure t
;;   :config
;;   (progn
;;     (emms-standard)
;;     (emms-default-players)
;;     (setq emms-directory "~/Music/网易云音乐/"))
;;   :bind (("C-<f5>" . emms-shuffle)
;;          ("C-<f6>" . emms-pause)
;;          ("C-<f7>" . emms-previous)
;;          ("C-<f8>" . emms-next)
;;          ("C-<f9>" . emms-volume-lower)
;; ("C-<f10>" . emms-volume-raise)))

;; (use-package elfeed
;;   :ensure t
;;   :defer t
;;   :bind
;;   (("C-c u f" . elfeed))
;;   :init
;;   (setq elfeed-feeds
;;         '(;;("https://news.ycombinator.com/rss" prog news)
;;           ("http://wanqu.co/feed/" prog news)))
;;   (setf url-queue-timeout 30))

;; (use-package el-pocket
;;   :ensure t
;;   :init (el-pocket-load-auth))

;; (use-package project-explorer
;;   :ensure t)
;; (add-to-list 'load-path (concat user-emacs-directory "config/plug/project-explorer"))
;; (eval-after-load 'projectile
;;   '(progn
;;      (setq pe/cache-enabled t)
;;      (require 'project-explorer)
;;      (add-hook 'project-explorer-mode-hook '(lambda () (visual-line-mode -1)))))

;; System monitor
;; (use-package symon
;;   :ensure t
;;   :defer t
;;   :init
;;   (symon-mode)
;;   (apply 'concat (mapcar 'funcall (car symon--display-fns)))
;;   :config
;;   (setq symon-sparkline-type 'bounded
;;         symon-sparkline-thickness 1))

;; eww browser
;; (use-package eww
;;   :ensure t
;;   :defer t
;;   :config (progn (setq browse-url-browser-function 'eww-browse-url)))

;; w3m
;; (use-package w3m
;;   :ensure t
;;   :defer t
;;   :bind ("C-c C-t m" . browse-url-at-point)
;;   :config (progn (setq browse-url-browser-function 'w3m-browse-url
;;                        w3m-use-cookies t
;;                        w3m-home-page "https://segmentfault.com")))
