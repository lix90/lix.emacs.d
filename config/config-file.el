;;; file mangament

(use-package flx-ido :ensure t :defer t :disabled t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  )

(use-package dired+ :ensure t :defer t)
(use-package dired-single :ensure t :defer t)
(use-package dired-details+ :defer t :ensure dired-details)

(bind-key* "C-S-d" 'kill-word)
(bind-keys :prefix-map lix/dired-map
           :prefix "M-d"
           :prefix-docstring "Dired Map"
           ("j" . dired-jump)
           ("d" . ido-dired))

(add-hook 'dired-mode-hook
          (lambda ()
            (put 'dired-find-alternate-file 'disabled nil)
            (dired-hide-details-mode t)
            (hl-line-mode t)
            ;; allow dired to delete or copy dir
            (setq dired-details-hidden-string " ")
            (setq dired-recursive-copies 'always) ; “always” means no asking
            (setq dired-recursive-deletes 'top) ; “top” means ask once
            (setq insert-directory-program (executable-find "gls"))
            (setq dired-dwim-target t)
            (define-key dired-mode-map (kbd "RET")
              'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))

;; (add-hook 'dired-load-hook
;;           (lambda()
;;             (load "dired-x")))

(use-package neotree :ensure t
  :commands (neotree-toggle)
  :bind (("<f7>" . neotree-toggle)
         :map neotree-mode-map
         ("RET" . neotree-enter)
         ("TAB" . neotree-stretch-toggle)
         ("|" . neotree-enter-vertical-split)
         ("-" . neotree-enter-horizontal-split)
         ("'" . neotree-quick-look)
         ("c" . neotree-create-node)
         ("C" . neotree-copy-node)
         ("d" . neotree-dir)
         ("D" . neotree-delete-node)
         ("g" . neotree-refresh)
         ("h" . spacemacs/neotree-collapse-or-up)
         ("H" . neotree-select-previous-sibling-node)
         ("n" . neotree-next-line)
         ("N" . neotree-down-node)
         ("p" . neotree-previous-line)
         ("P" . neotree-select-up-node)
         ("l" . spacemacs/neotree-expand-or-open)
         ("L" . neotree-select-next-sibling-node)
         ("q" . neotree-hide)
         ("R" . neotree-rename-node)
         ("r" . neotree-change-root)
         ("s" . neotree-hiden-file-toggle)
         )
  :config
  (setq-default neo-keymap-style 'concise)
  ;;(setq-default neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-theme 'icon)
  (setq  neo-show-hidden-files t
         neo-modern-sidebar t
         neo-show-updir-line nil
         neo-dont-be-alone t
         neo-banner-message nil
         neo-create-file-auto-open t
         neo-window-width 24
         neo-window-fixed-size t
         neo-smart-open t
         neo-auto-indent-point t
         neo-confirm-delete-directory-recursively t
         neo-confirm-delete-file t
         neo-vc-integration nil
         neo-mode-line-type 'none)
  ;; Solved this problem
  ;; https://github.com/jaypei/emacs-neotree/issues/50
  (setq-default neo-persist-show t)
  (when neo-persist-show
    (add-hook 'popwin:before-popup-hook
              (lambda () (setq neo-persist-show nil)))
    (add-hook 'popwin:after-popup-hook
              (lambda () (setq neo-persist-show t))))
             ;;; Code retrieved from https://github.com/jaypei/emacs-neotree/issues/218

  (defun lix/text-scale-twice ()
    "Text scale for neotree."
    (interactive)
    (progn
      (text-scale-adjust 0)
      (text-scale-decrease 1)))

  (add-hook 'neo-after-create-hook
            (lambda (_)
              ;;(call-interactively 'lix/text-scale-twice)
              (visual-line-mode -1)
              (setq truncate-lines t)))

  (defun spacemacs/neotree-expand-or-open ()
    "Expand or open a neotree node."
    (interactive)
    (let ((node (neo-buffer--get-filename-current-line)))
      (when node
        (if (file-directory-p node)
            (progn
              (neo-buffer--set-expand node t)
              (neo-buffer--refresh t)
              (when neo-auto-indent-point
                (next-line)
                (neo-point-auto-indent)))
          (call-interactively 'neotree-enter)))))

  (defun spacemacs/neotree-collapse ()
    "Collapse a neotree node."
    (interactive)
    (let ((node (neo-buffer--get-filename-current-line)))
      (when node
        (when (file-directory-p node)
          (neo-buffer--set-expand node nil)
          (neo-buffer--refresh t))
        (when neo-auto-indent-point
          (neo-point-auto-indent)))))

  (defun spacemacs/neotree-collapse-or-up ()
    "Collapse an expanded directory node or go to the parent node."
    (interactive)
    (let ((node (neo-buffer--get-filename-current-line)))
      (when node
        (if (file-directory-p node)
            (if (neo-buffer--expanded-node-p node)
                (spacemacs/neotree-collapse)
              (neotree-select-up-node))
          (neotree-select-up-node)))))

  (defun neotree-find-project-root ()
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (let ((origin-buffer-file-name (buffer-file-name)))
        (neotree-find (projectile-project-root))
        (neotree-find origin-buffer-file-name))))

  (defun spacemacs//neotree-maybe-attach-window ()
    (when (get-buffer-window (neo-global--get-buffer))
      (neo-global--attach))))

(provide 'config-file)
