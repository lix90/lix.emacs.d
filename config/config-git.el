;;; Git in Emacs
(use-package magit :ensure t :defer t
  :commands (magit-blame-mode
             magit-commit
             magit-diff
             magit-log
             magit-status)
  :config
  (progn
    (setq vc-follow-symlinks t)
    ;; make magit go fullscreen
    ;; (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    (setq magit-diff-refine-hunk 'all
          magit-push-always-verify nil)
    (global-git-commit-mode t) ; use emacs as editor for git commits
    ))

(use-package magit-gitflow :ensure t :defer t :after magit
  :init
  (add-hook 'magit-mode-hook #'turn-on-magit-gitflow))

(use-package git-timemachine :ensure t :defer t
  :commands git-timemachine)

;; ;; If you enable global minor mode
;;   (add-hook 'magit-pre-refresh-hook 'git-gutter+-refresh)
;; (global-git-gutter+-mode t)

;; (use-package magithub
;;   :ensure t
;;   :after magit)

(use-package gitignore-mode :ensure t :defer t)
(use-package gitconfig-mode :ensure t :defer t)
(use-package gitattributes-mode :ensure t :defer t)

(use-package git-gutter+ :ensure t :defer t
  :config
  (add-hook 'prog-mode-hook #'git-gutter+-mode)
  (set-face-foreground 'git-gutter+-added    "royal blue")
  (set-face-foreground 'git-gutter+-modified hl-color)
  (set-face-foreground 'git-gutter+-deleted  "hot pink"))

(use-package git-gutter-fringe+ :ensure t :after git-gutter+ :defer t
  :if (display-graphic-p)
  :config
  (require 'git-gutter-fringe+)
  (setq git-gutter-fr+-side 'right-fringe)
  (define-fringe-bitmap 'git-gutter-fr+-added
    ;;[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    [248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr+-modified
    ;;[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    [248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr+-deleted
    ;;[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    [248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248]
    nil nil 'center))

(provide 'config-git)
