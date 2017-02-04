;;; Git in Emacs
(use-package magit
  :ensure t
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
    (setq magit-diff-refine-hunk 'all)
    (global-git-commit-mode t) ; use emacs as editor for git commits
    (setq magit-push-always-verify nil)
    (add-hook 'git-commit-mode-hook 'turn-on-flyspell)

    (use-package evil-magit
      :ensure t
      :defer t
      :after magit)

    (use-package magit-gitflow
      :ensure t
      :defer t
      :after magit
      :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

    ))

;; (use-package magit
;;   :ensure t
;;   :bind ("C-x g" . magit-status)
;;   :config
;;   (progn (use-package evil-magit :ensure t)))


(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :defer t
  :commands git-timemachine
  )

(use-package git-gutter+
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'markdown-mode-hook #'git-gutter+-mode)
    (add-hook 'prog-mode-hook #'git-gutter+-mode))
  :config
  (progn
    (set-face-foreground 'git-gutter+-modified "yellow")
    (set-face-foreground 'git-gutter+-added    "blue")
    (set-face-foreground 'git-gutter+-deleted  "white")))

(use-package git-gutter-fringe+
  :ensure t
  :defer t
  :commands git-gutter+-mode
  :init
  (progn
    (when (display-graphic-p)
      (with-eval-after-load 'git-gutter+
        (require 'git-gutter-fringe+)))
    (setq git-gutter-fr+-side 'left-fringe)))

(defun quick-commit ()
  "make a quick commit from the mini-buffer"
  (interactive)
  (evil-ex '"!Git add % && Git commit -m '" ))

;; ;; If you enable global minor mode
;;   (add-hook 'magit-pre-refresh-hook 'git-gutter+-refresh)
;; (global-git-gutter+-mode t)))

;; (use-package magithub
;;   :ensure t
;;   :after magit)

(provide 'config-version-control)
