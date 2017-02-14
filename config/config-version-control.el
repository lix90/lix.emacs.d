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

    (use-package evil-magit :ensure t :defer t :after magit)
    (use-package magit-gitflow :ensure t :defer t :after magit
      :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))
    ))

(use-package git-timemachine :ensure t :defer t :commands git-timemachine)

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

(use-package gitignore-mode :ensure t :defer t)
(use-package gitconfig-mode :ensure t :defer t)
(use-package gitattributes-mode :ensure t :defer t)

(provide 'config-version-control)
