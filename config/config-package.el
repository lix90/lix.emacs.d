(use-package el-get :ensure t :defer t
  :init
  (setq el-get-verbose t))

(use-package paradox :ensure t :defer 30
  :commands (paradox-list-packages
             paradox-upgrade-packages)
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package package-utils :ensure t :defer 30
  :commands
  (package-utils-upgrade-by-name))

(provide 'config-package)
