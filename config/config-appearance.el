;;; Config-appearance configuration file:
;;; Commentary:
;;; Code:



(use-package atom-one-dark-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
;; (use-package twilight-bright-theme :ensure t :defer t :disabled t)
(use-package monokai-theme :ensure t :defer t)
(use-package spacemacs-common :ensure spacemacs-theme :defer t :disabled t) 

(use-package doom-neotree :ensure doom-themes :after neotree
  :config
  (setq doom-neotree-line-spacing 1
        doom-neotree-enable-file-icons t
        doom-neotree-folder-size 0.8))

;;; my mode-line-format
(use-package lix-mode-line :load-path "elisp")

(advice-add 'load-theme :after 'lix/update-faces)
;;(load-theme 'spacemacs-dark t)
(if (display-graphic-p)
    (load-theme 'doom-one t))

(use-package gruvbox-theme :ensure t :defer t)
(if (not (display-graphic-p))
    (load-theme 'spacemacs-dark t))

(provide 'config-appearance)
;;; config-appearance.el ends here
