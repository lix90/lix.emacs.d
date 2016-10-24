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
  (progn (use-package evil-magit :ensure t)
         ))

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

(defun lix/open-hexo-root ()
  "Open hexo root directory."
  (interactive)
  (find-file "~/github/hexo-blog/"))

(defun lix/open-hexo-source-draft ()
  "Open hexo draft directory."
  (interactive)
  (find-file "~/github/hexo-blog/source/_drafts/"))

(defun lix/open-hexo-source-post ()
  "Open hexo draft directory."
  (interactive)
  (find-file "~/github/hexo-blog/source/_posts/"))

(defun lix/open-jirengu-self ()
  "Open jirengu repository."
  (interactive)
  (find-file "~/jirengu/jrg-renwu9/homework/李想/"))

;; dicts
(use-package bing-dict
  :ensure t
  :defer t
  :bind ("C-c C-d b" . bing-dict-brief))

(use-package osx-dictionary
  :ensure t
  :defer t
  :bind (("C-c C-d a" . osx-dictionary-search-pointer)
         ("C-c C-d i" . osx-dictionary-search-input))
  :config (progn (setq osx-dictionary-use-chinese-text-segmentation t
                       osx-dictionary-dictionary-choice "Apple")))

(use-package youdao-dictionary
  :ensure t
  :defer t
  :bind ("C-c C-d y" . youdao-dictionary-search-at-point))

(provide 'config-tool)
