;; configure shell
(use-package eshell-did-you-mean :ensure t :defer t)
(use-package eshell-up :ensure t :defer t
  :commands (eshell-up eshell-up-peek)
  :config
  (progn
    (setq eshell-up-ignore-case nil)
    (setq eshell-up-print-parent-dir t)))
(use-package eshell-autojump :ensure t :defer t
  :commands (eshell/j))
(use-package eshell-z :ensure t :defer t
  :commands (eshell-z))

(use-package eshell :ensure t :defer t :commands eshell
  :config
  (progn
    (require 'em-cmpl)
    (require 'em-prompt)
    (require 'em-term)
    (setq
     eshell-highlight-prompt nil
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
     eshell-banner-message ""
     ;; eshell-banner-message "What would you like to do?\n\n"
     )
    ;; Visual commands
    (setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                                   "ncftp" "pine" "tin" "trn" "elm" "vim"
                                   "nmtui" "alsamixer" "htop" "el" "elinks"
                                   ))
    (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
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

    (defun my/setup-eshell ()
      (interactive)
      ;; turn off semantic-mode in eshell buffers
      (semantic-mode -1)
      ;; turn off hl-line-mode
      (hl-line-mode -1)
      (eshell-did-you-mean-setup))

    (add-hook 'eshell-mode-hook
              (lambda ()
                (my/setup-eshell)
                (eshell-cmpl-initialize)
                ))

    (defcustom dotemacs-eshell/prompt-git-info
      t
      "Turns on additional git information in the prompt."
      :group 'dotemacs-eshell
      :type 'boolean)

    ;; (epe-colorize-with-face "abc" 'font-lock-comment-face)
    (defmacro epe-colorize-with-face (str face)
      `(propertize ,str 'face ,face))

    (setq eshell-prompt-function
          (lambda ()
            (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
                    (when (and dotemacs-eshell/prompt-git-info
                               (fboundp #'vc-git-branches))
                      (let ((branch (car (vc-git-branches))))
                        (when branch
                          (concat
                           (propertize " [" 'face 'font-lock-keyword-face)
                           (propertize branch 'face 'font-lock-function-name-face)
                           (let* ((status (shell-command-to-string "git status --porcelain"))
                                  (parts (split-string status "\n" t " "))
                                  (states (mapcar #'string-to-char parts))
                                  (added (count-if (lambda (char) (= char ?A)) states))
                                  (modified (count-if (lambda (char) (= char ?M)) states))
                                  (deleted (count-if (lambda (char) (= char ?D)) states)))
                             (when (> (+ added modified deleted) 0)
                               (propertize (format " +%d ~%d -%d" added modified deleted) 'face 'font-lock-comment-face)))
                           (propertize "]" 'face 'font-lock-keyword-face)))))
                    (when (and (boundp #'venv-current-name) venv-current-name)
                      (concat
                       (epe-colorize-with-face " [" 'epe-venv-face)
                       (propertize venv-current-name 'face `(:foreground "#2E8B57" :slant italic))
                       (epe-colorize-with-face "]" 'epe-venv-face)))
                    (propertize " $ " 'face 'font-lock-constant-face))))))

(use-package shell-switcher :ensure t :disabled t
  :config
  (shell-switcher-mode t)
  (add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell))

(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
          '(lambda()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)))


(provide 'config-shell)
;;; config-shell.el ends here
