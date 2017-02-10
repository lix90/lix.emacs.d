;;; upgrade-packages
(use-package paradox
  :ensure t
  :commands (paradox-list-packages
             paradox-upgrade-packages)
  :defer 30
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package package-utils :ensure t
  :commands (package-utils-upgrade-by-name))

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


(provide 'config-utilities)
