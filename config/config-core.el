(require 'cl)
(setq package-enable-at-startup nil)
(when (>= emacs-major-version 24)
  (require 'package)
  ;;(setq package-archives '(("gnu" . "http://elpa.zilongshanren.com/gnu/")
  ;;                         ("melpa" . "http://elpa.zilongshanren.com/melpa/"))
  (add-to-list
   'package-archives
   ;;'(("gnu" . "https://elpa.zilongshanren.com/gnu/")
   ;;   ("melpa" . "https://elpa.zilongshanren.com/melpa/"))
   '("melpa" . "http://melpa.org/packages/")
   ;; '("popkit" . "https://elpa.popkit.org/packages/")
   ;;'("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/help/elpa/")
   t)
  (package-initialize))

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq package-user-dir (local-file-name "elpa"))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package auto-package-update
  :ensure t
  :init
  (progn
    (setq auto-package-update-interval 14)
    (setq auto-package-update-delete-old-versions t)
    (add-hook 'auto-package-update-before-hook
              (lambda () (message "I will update packages now.")))))

(use-package paradox
  :ensure t
  :defer t
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package esup :ensure t :defer 5)

(provide 'config-core.el)
;;; config-package.el ends here
