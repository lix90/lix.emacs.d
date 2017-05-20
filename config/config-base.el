;; 键位绑定
(use-package general :ensure t :defer t
  :init
  (general-create-definer
   leader-key
   :keymaps 'global
   :prefix (if is-mac "H-SPC"
             "s-SPC")))

;; 更方便地导航
(use-package god-mode :ensure t :defer t
  :config
  (defun lix/update-cursor ()
    (setq cursor-type
          (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'god-mode-enabled-hook 'lix/update-cursor)
  (add-hook 'god-mode-disabled-hook 'lix/update-cursor))
(global-set-key (kbd "<escape>") 'god-local-mode)

;; 功能更加强劲的minibar
(use-package which-key :ensure t
  :init
  (which-key-setup-minibuffer)
  (which-key-mode t)
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.30
        which-key-side-window-max-width 0.20
        which-key-max-description-length 25
        which-key-allow-evil-operators t
        which-key-sort-order 'which-key-key-order
        which-key-unicode-correction 3
        which-key-prefix-prefix "+"
        which-key-idle-delay 0.15))

;; 一些基本的键位绑定
(unbind-key "C-SPC")
(global-set-key (kbd "C-:") 'set-mark-command)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-s s") 'isearch-forward-regexp)
(global-set-key (kbd "M-s r") 'isearch-backward-regexp)
(leader-key "<SPC>" 'counsel-M-x
            "h" 'ivy-resume
            "i" 'ivy-imenu-anywhere
            "k" 'which-key-show-top-level
            "r" 'counsel-recentf
            "M" 'woman
            "." 'quick-commit
            ";" 'comment-or-uncomment-region
            "TAB" 'switch-to-previous-buffer
            "D" 'lix/restore-desktop
            "O" 'counsel-osx-app
            "=" 'er/expand-region)

(provide 'config-base)
