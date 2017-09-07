;;;--------------------
;; Documentation
;;;--------------------
(use-package help-fns+ :ensure t :defer t
  :bind (("C-h M-k" . describe-keymap)))

;; Bookmark+
(use-package bookmark+ :ensure t :defer t)

;; Make comments invisible
(use-package nocomments-mode :ensure t :defer t
  :commands (nocomments-mode))

;; Open OSX apps
(use-package counsel-osx-app :ensure t :defer t :if is-mac
  :commands (counsel-osx-app))

(use-package restart-emacs :ensure t :defer t
  :commands restart-emacs)

(use-package vbasense :ensure t :defer t :disabled t)

(use-package bing-dict :ensure t :defer t)
(use-package youdao-dictionary :ensure t :defer t)
(use-package wotd :ensure t :defer t)

(provide 'config-misc)
