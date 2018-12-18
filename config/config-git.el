;;; config-git.el --- Configuration for git version control:

;;; Commentary: lix的magit配置文件。包括：
;;;
;;; Packages:
;;; * Magit 主要git工具包
;;; * magit-gitflow 快速使用gitflow弹窗菜单
;;; * git-timemachine 可以快速查看文件历史状态
;;; * gitignore-mode 对 `.gitignore` 文件进行高亮
;;; * gitconfig-mode 对 `.gitconfig` 文件进行高亮
;;; ------------------------------------------------------
;;; keybindings:
;;; Globally:
;;; C-c m s 全局查看项目修改状态
;;; C-c m p 全局调出magit调度弹窗
;;; Locally:
;;; C-f 在magit status buffer中按C-f将显示gitflow弹窗菜单
;;; ------------------------------------------------------

;;; Code:
;;; Git in Emacs
(use-package magit :ensure t :defer t
  :bind (("C-c m s" . magit-status)
         ("C-c m p" . magit-dispatch-popup))
  :config
  (setq vc-follow-symlinks t)
  (setq magit-diff-refine-hunk 'all
        magit-push-always-verify nil
        magit-commit-show-diff nil)
  (global-git-commit-mode t)
  )

;; 在magit status buffer中，按C-f将显示gitflow弹窗菜单
(use-package magit-gitflow :ensure t :defer t :after magit
  :init
  (add-hook 'magit-mode-hook #'turn-on-magit-gitflow))

(use-package git-timemachine :ensure t :defer t
  :commands git-timemachine)

(use-package gitignore-mode :ensure t :defer t)
(use-package gitconfig-mode :ensure t :defer t)
(use-package gitattributes-mode :ensure t :defer t)

(use-package git-gutter+ :ensure t :defer t :disabled t
  :config
  (add-hook 'prog-mode-hook #'git-gutter+-mode)
  (set-face-foreground 'git-gutter+-added    "royal blue")
  (set-face-foreground 'git-gutter+-modified hl-color)
  (set-face-foreground 'git-gutter+-deleted  "hot pink"))

(use-package git-gutter-fringe+ :ensure t :after git-gutter+ :defer t :disabled t
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
;;; config-git.el ends here













