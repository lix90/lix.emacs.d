;;; package --- Summary:

;;; Commentary:

;;; Code;

;;; WOMAN shell命令文档
;;;============================================================
(autoload 'woman "woman"
  "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman"
  "Find, decode and browse a specific UN*X man-page file." t)

;;; eshell配置
;;;============================================================
(use-package eshell :defer t :no-require t
  :config
  (setq eshell-highlight-prompt t
        eshell-buffer-shorthand t
        eshell-cmpl-ignore-case t
        eshell-cmpl-cycle-completions nil
        eshell-history-size 500
        eshell-buffer-maximum-lines 12000
        eshell-hist-ignoredups t
        eshell-error-if-no-glob t
        eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input 'all
        eshell-list-files-after-cd nil
        eshell-aliases-file (concat user-emacs-directory "eshell/alias")
        eshell-banner-message ""
        eshell-bad-command-tolerance 3)
  (setq eshell-visual-commands
        '("vi" "screen" "top" "less" "more" "lynx"
          "ncftp" "pine" "tin" "trn" "elm" "vim"
          "nmtui" "alsamixer" "htop" "el" "elinks")
        eshell-visual-subcommands '(("git" "log" "diff" "show")))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (global-hl-line-mode -1)
              (set (make-local-variable 'company-backends)
                   '(company-files))
              (company-mode +1)
              (eshell-cmpl-initialize)))
  ;; 尝试解决在eshell中npm命令的结果显式乱码
  ;; http://stackoverflow.com/questions/13185729/npm-dont-display-prompt-correctly-under-emacs-eshell
  (setenv "NODE_NO_READLINE" "1"))

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

;;; shell配置
;;;============================================================
(use-package shell :defer t
  :config
  (add-hook 'shell-mode-hook (lambda() (global-hl-line-mode -1)))
  (setq explicit-shell-file-name "bash"
        shell-file-name explicit-shell-file-name
        explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
        comint-process-echoes t
        explicit-bash-args '("--login" "--init-file" "$HOME/.bash_profile" "-i"))
  (setenv "ESHELL" shell-file-name)
  ;; http://www.joshstaiger.org/archives/2005/07/fixing_garbage.html
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook #'company-mode))

;;; shell切换
;;; C-x 4 ' 在另外的窗口打开shell
;;; C-x 4 s 在当前窗口打开shell
(use-package shell-switcher :ensure t :defer t
  :bind (("M-s C-S" . shell-switcher-switch-buffer-other-window)
         ("M-s C-s" . shell-switcher-new-shell)
         ("M-s C-b" . shell-switcher-switch-buffer))
  :config
  (setq shell-switcher-new-shell-function 'shell-switcher-make-shell)
  (setq shell-switcher-ansi-term-shell (if is-mac "/usr/local/bin/bash" "/bin/bash"))
  (unbind-key "C-'" shell-switcher-mode-map)
  (unbind-key "C-M-'" shell-switcher-mode-map))

;;; 编辑shell脚本
(use-package sh-script
  :mode (("\\.?zshrc$" . shell-script-mode)
         ("\\.?bashrc$" . shell-script-mode)
         ("\\.?bash_profile$" . shell-script-mode)
         ("\\.profile$" . shell-script-mode)
         ("\\.?alias$" . shell-script-mode)
         ("\\.?aliases$" . shell-script-mode)))

(use-package company-shell :ensure t :defer t :after sh-script
  :config
  (add-to-list 'company-backends
               '(company-shell company-shell-env company-fish-shell)))

(use-package quickrun :ensure t :defer t)

;;; COMINT
;;;============================================================
(use-package comint :defer t
  :config
  (setq
   ;; 总是在底端插入命令
   comint-scroll-to-bottom-on-input t
   ;; 命令历史去重
   comint-input-ignoredups t)
  ;; Avoid Emacs querying "active processes exist;
  ;; kill them and exit anyway?", since we are creating
  ;; an inferior python process and aspell
  (add-hook 'comint-exec-hook
            (lambda ()
              (set-process-query-on-exit-flag
               (get-buffer-process (current-buffer)) nil))))

;;; TRAMP：与远程主机交互
;;;============================================================
;;; 基本使用：
;;; C-x C-f /remotehost:filename RET
;;; (or /method:user@remotehost:filename)
;;;  C-x C-f /su::/etc/hosts
;;;  C-x C-f /sudo::/etc/hosts
(use-package tramp :defer t
  :config
  (setq tramp-default-method "ssh")
  (setenv "SHELL" (if is-mac "/usr/local/bin/bash" "/bin/bash")))

(provide 'config-shell)
;;; config-shell.el ends here
