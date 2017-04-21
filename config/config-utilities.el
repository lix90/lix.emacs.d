;; Bookmark+
(use-package bookmark+ :ensure t :defer t)

;; Make comments invisible
(use-package nocomments-mode :ensure t :defer t
  :commands (nocomments-mode))

;; Open OSX apps
(use-package counsel-osx-app :ensure t :defer t
  :commands (counsel-osx-app))

(use-package restart-emacs :ensure t :defer 20
  :commands restart-emacs)

(use-package help-fns+ :ensure t :defer 5)

;; dicts
(use-package bing-dict :ensure t :defer 20
  :commands (bing-dict-brief)
  :init (defalias 'Bing-dictionary 'bing-dict-brief))

(use-package youdao-dictionary :ensure t :defer 20
  :commands (youdao-dictionary-search-at-point)
  :init (defalias 'Youdao-dictionary 'youdao-dictionary-search-at-point))


;;; ------------------------------------------------------------------
;;; upgrade-packages
(use-package popup :ensure t :defer t :disabled t
  :config
  ;; Change popup tip face
  ;; TODO make popup tip scrollable
  (set-face-attribute 'popup-tip-face nil
                      :foreground "#aaaaaa"
                      :background "#282c32")

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

(provide 'config-utilities)
