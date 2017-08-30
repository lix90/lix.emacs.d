(use-package el-get :ensure t :defer t
  :init
  (setq el-get-verbose t))

(use-package paradox :ensure t :defer 30
  :commands (paradox-list-packages
             paradox-upgrade-packages)
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(setq paradox-github-token "d02fae45dd7c0c4845b56635d78448c63d7a7035")

(use-package package-utils :ensure t :defer 30
  :commands
  (package-utils-upgrade-by-name))

(provide 'config-package)
