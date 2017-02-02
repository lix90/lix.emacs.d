;; configure shell
;; (use-package shell-pop
;;   :ensure t
;;   :init
;;   (progn
;;     (setq shell-pop-shell-type (quote ("shell" "*shell*" (lambda nil (shell)))))
;;     (setq shell-pop-term-shell "/bin/bash")
;;     (setq shell-pop-universal-key "C-c u z")
;;     (setq shell-pop-window-size 30)
;;     (setq shell-pop-full-span t)
;;     (setq shell-pop-window-position "bottom")))

(use-package eshell
  :ensure t
  :commands eshell
  ;; :bind ("C-x e" . eshell)
  :config
  (use-package em-cmpl :ensure nil)
  (use-package em-prompt :ensure nil)
  (use-package em-term :ensure nil)
  (setq
   eshell-highlight-prompt nil
   eshell-buffer-shorthand t
   eshell-cmpl-ignore-case t
   eshell-cmpl-cycle-completions nil
   eshell-history-size 500
   ;; auto truncate after 12k lines
   eshell-buffer-maximum-lines 12000
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t
   eshell-glob-case-insensitive t
   eshell-scroll-to-bottom-on-input 'all
   eshell-list-files-after-cd t
   ;;   eshell-aliases-file (concat user-emacs-directory "eshell/alias")
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
  )

(defun my/setup-eshell ()
  (interactive)
  ;; turn off semantic-mode in eshell buffers
  (semantic-mode -1)
  ;; turn off hl-line-mode
  (hl-line-mode -1))

(add-hook 'eshell-mode-hook
          (lambda ()
            (my/setup-eshell)
            (eshell-cmpl-initialize)))

(defcustom dotemacs-eshell/prompt-git-info
  t
  "Turns on additional git information in the prompt."
  :group 'dotemacs-eshell
  :type 'boolean)

;; (epe-colorize-with-face "abc" 'font-lock-comment-face)
(defmacro epe-colorize-with-face (str face)
  `(propertize ,str 'face ,face))

;; (defface epe-venv-face
;;   '((t (:inherit font-lock-comment-face)))
;;   "Face of python virtual environment info in prompt."
;;   :group 'epe)

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
                (propertize " $ " 'face 'font-lock-constant-face))))


(use-package shell-switcher
  :ensure t
  :config
  (setq shell-switcher-mode t)
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

(defun eshell/magit ()
  "Function to open magit-status for the current directory"
  (interactive)
  (magit-status default-directory)
  nil)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :defer t
  :init
  (progn
    ;; (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PATH")
    ;; Solve warning of setting locale in ESS
    ;; from: https://stat.ethz.ch/pipermail/r-sig-mac/2015-October/011701.html
    (exec-path-from-shell-copy-env "LC_ALL")
    (exec-path-from-shell-copy-env "LANG")))

;; (use-package shell-pop
;;   :ensure t
;;   :commands shell-pop
;;   :init
;;   (setq
;;    shell-pop-term-shell "/usr/local/bin/bash"
;;    shell-pop-window-size 20
;;    shell-pop-full-span t
;;    ;;shell-pop-default-directory "/Users/lix/"
;;    shell-pop-window-position "bottom"))

;; (defun ansi-term-handle-close ()
;;   "Close current term buffer when `exit' from term buffer."
;;   (when (ignore-errors (get-buffer-process (current-buffer)))
;;     (set-process-sentinel (get-buffer-process (current-buffer))
;;                           (lambda (proc change)
;;                             (when (string-match "\\(finished\\|exited\\)" change)
;;                               (kill-buffer (when (buffer-live-p (process-buffer proc)))
;;                                            (delete-window))))))
;;   (add-hook 'shell-pop-out-hook 'kill-this-buffer)
;;   (add-hook 'term-mode-hook (lambda () (linum-mode -1) (ansi-term-handle-close))))

(setq shell-file-name "/usr/local/bin/bash")
(setq explicit-bash-args '("--login" "--init-file" "$HOME/.bash_profile" "-i"))
(defalias 'sh 'shell)

;; set coding system
;; (setenv "LANG" "zh_CN.UTF-8")

(provide 'config-shell)
;;; config-shell.el ends here
