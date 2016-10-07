
;;; keycheatsheet --- which-key:
;;; Commentary:
;;; Code:
(use-package which-key
  :ensure t
  :init
  (progn
    (which-key-mode)
    (which-key-setup-side-window-bottom))
  :diminish which-key-mode)

(provide 'config-whichkey)
;;; config-whichkey.el ends here
