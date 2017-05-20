;;;-------------------------
;; with editor
;;;-------------------------
(use-package with-editor :ensure t :defer t
  :init
  (define-key (current-global-map)
    [remap async-shell-command] 'with-editor-async-shell-command)
  (define-key (current-global-map)
    [remap shell-command] 'with-editor-shell-command)
  :config
  (progn
    (add-hook 'shell-mode-hook  #'with-editor-export-editor)
    (add-hook 'term-exec-hook   #'with-editor-export-editor)
    (add-hook 'eshell-mode-hook #'with-editor-export-editor)
    (add-hook 'shell-mode-hook #'with-editor-export-git-editor)
    (add-hook 'eshell-mode-hook #'with-editor-export-git-editor)
    ))


;;;-------------------------
;; eshell
;;;-------------------------
;;(use-package eshell-did-you-mean :ensure t :defer t)
(use-package eshell-up :ensure t :defer t :disabled t
  :commands (eshell-up eshell-up-peek)
  :config
  (setq eshell-up-ignore-case nil
        eshell-up-print-parent-dir t))
(use-package eshell-autojump :ensure t :defer t :disabled t
  :commands (eshell/j))
(use-package eshell-z :ensure t :defer t :disabled t
  :commands (eshell-z))

(use-package eshell :ensure t :defer t :commands eshell
  :config
  (setq eshell-highlight-prompt t
        eshell-buffer-shorthand t
        eshell-cmpl-ignore-case t
        eshell-cmpl-cycle-completions nil
        eshell-history-size 500
        eshell-buffer-maximum-lines 12000 ; auto truncate after 12k lines
        eshell-hist-ignoredups t
        eshell-error-if-no-glob t
        eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input 'all
        eshell-list-files-after-cd nil
        eshell-aliases-file (concat user-emacs-directory "eshell/alias")
        eshell-banner-message ""
        eshell-bad-command-tolerance 3)
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

  (add-hook 'eshell-mode-hook
            (lambda ()
              (semantic-mode nil)
              (hl-line-mode nil)
              (set (make-local-variable 'company-backends)
                   '(company-files))
              (company-mode t)
              ;;(eshell-did-you-mean-setup)
              (eshell-cmpl-initialize))))

;; npm don't display prompt correctly under emacs eshell
;; http://stackoverflow.com/questions/13185729/npm-dont-display-prompt-correctly-under-emacs-eshell
(setenv "NODE_NO_READLINE" "1")

;; Stripping stray ANSI escape sequences from eshell
;; https://emacs.stackexchange.com/questions/18457/stripping-stray-ansi-escape-sequences-from-eshell
;; How can I remove the ANSI escape sequences from a string in python
;; http://stackoverflow.com/questions/14693701/how-can-i-remove-the-ansi-escape-sequences-from-a-string-in-python
(defvar my-ansi-escape-re
  (rx (or ?\233 (and ?\e ?\[))
      (zero-or-more (char (?0 . ?\?)))
      (zero-or-more (char ?\s ?- ?\/))
      (char (?@ . ?~))))

(defun my-nuke-ansi-escapes (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward my-ansi-escape-re end t)
      (replace-match ""))))

(defun my-eshell-nuke-ansi-escapes ()
  (my-nuke-ansi-escapes eshell-last-output-start eshell-last-output-end))

(add-hook 'eshell-output-filter-functions 'my-eshell-nuke-ansi-escapes t)

;;; clear buffer, alternative to eshell/clear
(defun eshell/clr()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;;(eshell-send-input)
    ))

;;(use-package eshell-config :load-path "elisp")

(use-package shell-switcher :ensure t :defer t
  :commands (shell-switcher-switch-buffer))
(leader-key "Iw" 'shell-switcher-switch-buffer)

(use-package eshell-prompt-extras :ensure t :after eshell
  :config
  (progn
    (venv-initialize-eshell)
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

;;; shell
(setq lix/shell-type "bash")
(when (or is-mac
          (string= lix/shell-type "bash"))
  (setq explicit-bash-args '("--login" "--init-file" "$HOME/.bash_profile" "-i")))
(setq shell-file-name (if is-mac "/usr/local/bin/bash"
                        (executable-find lix/shell-type))
      explicit-shell-file-name (if is-mac "/usr/local/bin/bash"
                                 (executable-find lix/shell-type)))

;;; shell-script-mode auto mode configuration
(add-to-list 'auto-mode-alist '("\\.?zsh\(rc\)?$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.?bashrc$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.?bash_profile$" . shell-script-mode))

(use-package multi-term
  :ensure t
  :defer t
  :init
  (setq multi-term-program-switches "--login"))

(use-package terminal-here :ensure t :defer t :disabled t)


(use-package shell :ensure t :defer t
  :config
  (add-hook 'shell-mode-hook #'company-mode)
  (use-package comint
    :config
    (setq comint-scroll-to-bottom-on-input t ; Always insert at the bottom
          ;; No duplicates in command history
          comint-input-ignoredups t))

  (use-package shell-command :ensure t
    :config (shell-command-completion-mode 1))

  ;; Set up shell (not eshell) mode: https://github.com/monsanto/readline-complete.el/blob/master/readline-complete.el
  ;; https://stackoverflow.com/questions/37409085/how-to-define-a-default-shell-for-emacs
  (setq-default explicit-shell-file-name "/bin/bash"
                shell-file-name explicit-shell-file-name
                explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
                comint-process-echoes t)
  (setenv "ESHELL" shell-file-name)

  (use-package readline-complete
    :ensure t
    :init
    (push 'company-readline company-backends)
    (add-hook 'rlc-no-readline-hook
              (lambda ()
                (company-mode -1))))

  (use-package bash-completion
    :ensure t
    :init (bash-completion-setup))

  ;; http://www.joshstaiger.org/archives/2005/07/fixing_garbage.html
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on))

(use-package term
  :disabled t
  :config
  (use-package term+
    :ensure t))

;; Avoid Emacs querying "active processes exist; kill them and exit anyway?", since we are creating an inferior python
;; process and aspell
(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(use-package zlc :ensure t :defer t :disabled t)

(provide 'config-shell)
;;; config-shell.el ends here
