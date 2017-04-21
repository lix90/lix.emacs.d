;; configure shell
;;(use-package eshell-did-you-mean :ensure t :defer t)
(use-package eshell-up :ensure t :defer t
  :commands (eshell-up eshell-up-peek)
  :config
  (setq eshell-up-ignore-case nil
        eshell-up-print-parent-dir t))
(use-package eshell-autojump :ensure t :defer t
  :commands (eshell/j))
(use-package eshell-z :ensure t :defer t
  :commands (eshell-z))
(use-package eshell :ensure t :defer t :commands eshell
  :config
  (setq eshell-highlight-prompt nil
        eshell-buffer-shorthand t
        eshell-cmpl-ignore-case t
        eshell-cmpl-cycle-completions nil
        eshell-history-size 500
        eshell-buffer-maximum-lines 12000 ; auto truncate after 12k lines
        eshell-hist-ignoredups t
        eshell-error-if-no-glob t
        eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input 'all
        eshell-list-files-after-cd t
        eshell-aliases-file (concat user-emacs-directory "eshell/alias")
        eshell-banner-message "")
  ;; Visual commands
  (setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                                 "ncftp" "pine" "tin" "trn" "elm" "vim"
                                 "nmtui" "alsamixer" "htop" "el" "elinks")
        eshell-visual-subcommands '(("git" "log" "diff" "show")))
  (defun my/truncate-eshell-buffers ()
    "Truncates all eshell buffers"
    (interactive)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (eq major-mode 'eshell-mode)
          (eshell-truncate-buffer)))))
  ;; After being idle for 5 seconds, truncate all the eshell-buffers if
  ;; needed. If this needs to be canceled, you can run `(cancel-timer
  ;; my/eshell-truncate-timer)'
  (setq my/eshell-truncate-timer
        (run-with-idle-timer 5 t #'my/truncate-eshell-buffers))

  (when (not (functionp 'eshell/rgrep))
    (defun eshell/rgrep (&rest args)
      "Use Emacs grep facility instead of calling external grep."
      (eshell-grep "rgrep" args t)))

  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (semantic-mode nil)
              (hl-line-mode nil)
              ;;(eshell-did-you-mean-setup)
              (eshell-cmpl-initialize)
              (local-set-key (kbd "C-l") 'eshell-clear-buffer))))

(use-package eshell-config :load-path "elisp")

(use-package shell-switcher :ensure t
  :config
  (shell-switcher-mode t)
  (add-hook 'eshell-mode-hook #'shell-switcher-manually-register-shell))

(provide 'config-shell)
;;; config-shell.el ends here
