;;; upgrade-packages
(use-package paradox
  :ensure t
  :commands (paradox-list-packages
             paradox-upgrade-packages)
  :defer 30
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package esup
  :ensure t
  :commands (esup)
  :defer 30)

(use-package restart-emacs
  :ensure t
  :defer 20
  :commands restart-emacs)

(use-package help-fns+ :ensure t :defer 5)

;; dicts
(use-package bing-dict
  :ensure t
  :commands (bing-dict-brief)
  :defer 20)

(use-package youdao-dictionary
  :ensure t
  :defer 20
  :commands (youdao-dictionary-search-at-point))

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

(provide 'config-utilities)
