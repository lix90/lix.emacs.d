(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-map (ansi-color-make-color-map) t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(fci-rule-color "#373b41")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-sexp-background-color "#efebe9")
 '(ibuffer-deletion-face (quote diredp-deletion-file-name))
 '(ibuffer-marked-face (quote diredp-flag-mark))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (chinese-fonts-setup evil-indent-plus counsel-dash counsel-osx-app aggressive-fill-paragraph nocomments-mode elfeed-goodies bookmark+ elfeed-org dash-at-point ess-R-object-popup ess-R-data-view ess-smart-underscore el-get company-ess em-prompt em-cmpl eshell-z eshell-autojump eshell-up eshell-did-you-mean gitattributes-mode gitconfig-mode gitignore-mode counsel-projectile evil-embrace google-this github-browse-file indent-tools yafolding avy-flycheck mmm-mode chinese-yasdcv chinese-pyim el2markdown slime elisp-slim-nav preview smooth-scrolling leuven-theme color-theme-molokai ample-theme color-theme-sanityinc-tomorrow package-utils dired+ diredful dired-details+ super-save imenu-anywhere evil-terminal-cursor-changer spacemacs-theme darktooth-theme peacock-theme liso-theme forest-blue-theme atom-one-dark-theme suscolors-theme creamsody-theme flyspell-popup nameless popwin uniquify benchmark-init flycheck-pos-tip git-gutter all-the-icons-dired all-the-icons mode-icons ox-pandoc toc-org ox-reveal org-inlinetask htmlize centered-cursor-mode enh-ruby-mode eclim clang-format disaster neotree persistent-scratch restart-emacs flycheck markdown-toc markdown-mode emmet-mode pug-mode psysh php-eldoc ac-php php-mode js-comint nodejs-repl web-beautify yaml-mode rainbow-mode scss-mode json-mode js2-mode lorem-ipsum company-web web-mode latex-preview-pane company-auctex auctex matlab-mode ein elpy company-anaconda anaconda-mode polymode key-combo ess org-bullets help-fns+ bing-dict evil-magit magit-gitflow magit which-key counsel flx rainbow-delimiters smartparens ace-jump-mode dired-details yasnippet company-statistics company solarized-theme better-defaults phi-rectangle crux move-text fix-word hungry-delete phi-search multiple-cursors whole-line-or-region expand-region undo-tree whitespace-cleanup-mode aggressive-indent esup paradox exec-path-from-shell use-package)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
