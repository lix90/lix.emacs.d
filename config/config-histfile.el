;;; Historyfile --- savehist, desktop, recentf:
;;; Commentary:
;;; Code:


;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (local-file-name "cache/savehist"))
(savehist-mode t)

(require 'desktop)
(setq-default desktop-missing-file-warning nil
              desktop-load-locked-desktop t
              desktop-restore-eager 0
              desktop-path `(,(local-file-name "cache"))
              desktop-save t)
(desktop-save-mode t)

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 30)
                (shell-command-history    . 50)
                tags-file-name
                register-alist))
      desktop-locals-to-save nil)
(desktop-read)

;; save recent files
(use-package recentf
  :init (recentf-mode t)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (progn
    (setq recentf-save-file (local-file-name "cache/recentf")
          recentf-max-saved-items 100
          recentf-max-menu-items 25)))

(provide 'config-histfile)

;;; config-fm.el ends here
