;;; Google Search In Emacs
;; (use-package google-this
;;   :ensure t
;;   :diminish google-this-mode
;;   :config
;;   (google-this-mode 1))

;;; Git in Emacs
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (progn (use-package evil-magit :ensure t)))

(use-package magit-gitflow
  :ensure t
  :after magit
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; (use-package magithub
;;   :ensure t
;;   :after magit)

;; open folders
(defun lix/open-emacs-config ()
  "Open emacs config directory."
  (interactive)
  (find-file "~/.emacs.d/config/"))
(global-set-key (kbd "C-c u o c") 'lix/open-emacs-config)

(defun lix/open-hexo-root ()
  "Open hexo root directory."
  (interactive)
  (find-file "~/github/hexo-blog/"))
(global-set-key (kbd "C-c u o h") 'lix/open-hexo-root)

(defun lix/open-hexo-source-draft ()
  "Open hexo draft directory."
  (interactive)
  (find-file "~/github/hexo-blog/source/_drafts/"))
(global-set-key (kbd "C-c u o d") 'lix/open-hexo-source-draft)

(defun lix/open-hexo-source-post ()
  "Open hexo draft directory."
  (interactive)
  (find-file "~/github/hexo-blog/source/_posts/"))
(global-set-key (kbd "C-c u o p") 'lix/open-hexo-source-post)

(defun lix/open-jirengu-self ()
  "Open jirengu repository."
  (interactive)
  (find-file "~/jirengu/jrg-renwu9/homework/李想/"))

(defun lix/file-hexo-source-about ()
  "Open hexo about."
  (interactive)
  (find-file "~/github/hexo-blog/source/about/index.md"))
(global-set-key (kbd "C-c u o a") 'lix/file-hexo-source-about)

(defun lix/file-note-temp ()
  "Open note temp file."
  (interactive)
  (find-file "~/github/temp.md"))
(global-set-key (kbd "C-c u o n") 'lix/file-note-temp)

;; dicts
(use-package bing-dict
  :ensure t
  :defer t
  :bind ("C-c u d b" . bing-dict-brief))

(use-package osx-dictionary
  :ensure t
  :defer t
  :bind (("C-c u d a" . osx-dictionary-search-pointer)
         ("C-c u d i" . osx-dictionary-search-input))
  :config (progn (setq osx-dictionary-use-chinese-text-segmentation t
                       osx-dictionary-dictionary-choice (list "Apple" "longman" "oxford"))))

(use-package youdao-dictionary
  :ensure t
  :defer t
  :bind ("C-c u d y" . youdao-dictionary-search-at-point))

(use-package google-translate
  :ensure t
  :defer t
  :bind (("C-c u d g" . google-translate-at-point)
         ("C-c u d G" . google-translate-smooth-translate)
         ("C-c u d r" . google-translate-at-point-reverse)
         ("C-c u d R" . google-translate-query-translate-reverse))
  :config
  (progn (require 'google-translate-smooth-ui)
         (setq google-translate-default-source-language "en"
               google-translate-default-target-language "zh"
               max-mini-window-height 0.5)
         (setq google-translate-translation-directions-alist
               '(("en" . "zh") ("zh" . "zh"))))
  )


;; project management
;; (use-package projectile
;;   :ensure t
;;   :init
;;   (projectile-global-mode))

(use-package sr-speedbar
  :ensure t
  :config
  (setq sr-speedbar-right-side nil)
  (setq sr-speedbar-auto-refresh nil)
  (setq sr-speedbar-width 40)
  (setq sr-speedbar-max-width 80)
  (setq speedbar-smart-directory-expand-flag t)
  (setq speedbar-use-images nil)
  (speedbar-add-supported-extension ".el")
  (add-hook 'speedbar-mode-hook #'(lambda () (visual-line-mode -1))))

(use-package project-explorer
  :ensure t
  :bind (("<f8>" . project-explorer-toggle)
         :map project-explorer-mode-map
         ("C-M-o" . pe/toggle-omit))
  ;; :config
  ;; (add-hook 'project-explorer-mode-hook (lambda ()
  ;;                                         (unbind-key "M-o" project-explorer-mode-map)))
  )

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
;;   :config (progn (setq symon-sparkline-type 'plain
;;                        symon-sparkline-thickness 1)))

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

(provide 'config-tool)
