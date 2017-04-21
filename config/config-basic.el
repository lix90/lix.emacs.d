;;; The differences between setq & setq-default
;; setq ---> current buffer
;; setq-default ---> global

(use-package better-defaults :ensure t :defer t)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      create-lockfiles nil
      ;; nice scrolling
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      cursor-in-non-selected-windows nil
      use-dialog-box nil
      ring-bell-function 'ignore
      buffer-file-coding-system 'utf-8-unix
      message-log-max 10000
      ;; do not backup files
      make-backup-files nil
      )

(setq-default indent-tabs-mode nil  ;; don't use tabs to indent
              tab-width 4           ;; but maintain correct appearance
              case-fold-search t    ;; case INsensitive search
              default-directory "~"
              fill-column 80
              next-line-add-newlines nil  ;; don't add new lines when scrolling down
              require-final-newline t     ;; end files with a newline
              mouse-yank-at-point t       ;; yank at cursor, NOT at mouse position
              kill-whole-line t)


(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(global-visual-line-mode t)
(global-subword-mode t)
(delete-selection-mode t)
(abbrev-mode t)
(setq save-abbrevs 'silently
      abbrev-file-name (concat user-cache-directory "emacs_abbre.el"))

(diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)
(diminish 'subword-mode)
(diminish 'delete-selection-mode)
(diminish 'abbrev-mode)

(when is-mac
  (setq mac-allow-anti-aliasing t
        delete-by-moving-to-trash t
        trash-directory "~/.Trash"
        ns-pop-up-frames nil
        ns-use-native-fullscreen nil)
  ;; Set modifier keys
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-function-modifier 'hyper
        mac-right-option-modifier 'none))

(add-hook 'after-init-hook
          (lambda ()
            (fringe-mode '(8 . 2)) ; Make fringe look good with git-gutter-fringe+
            ;; Set font
            (set-face-attribute 'default nil
                                :family "Source Code Pro"
                                :height 120
                                :weight 'normal
                                :width 'normal)))

;; Time stamps
(use-package time-stamp :ensure t :defer t
  :config
  (setq time-stamp-active t          ; do enable time-stamps
        time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
        time-stamp-format "Last modified on %04y-%02m-%02d %02H:%02M:%02S (%U)")
  (add-hook 'write-file-hooks 'time-stamp))

;;(setq display-time-format "%a %b %d | %H:%M |")
;;(display-time-mode)
(use-package exec-path-from-shell :ensure t :defer t
  :if (memq window-system '(mac ns))
  :init
  ;; Solve warning of setting locale in ESS
  ;; from: https://stat.ethz.ch/pipermail/r-sig-mac/2015-October/011701.html
  (exec-path-from-shell-copy-env "LC_ALL")
  (exec-path-from-shell-copy-env "LANG")
  (exec-path-from-shell-initialize))

(setq shell-file-name "/usr/local/bin/bash"
      explicit-bash-args '("--login" "--init-file" "$HOME/.bash_profile" "-i"))
;; (setenv "PATH" (concat "/Users/lix/anaconda3/bin/:/usr/local/bin:" (getenv "PATH")))
;; (setq exec-path (append '("/Users/lix/anaconda3/bin/"
;;                           "/usr/local/bin") exec-path))


(use-package super-save :ensure t :defer t
  :init
  (super-save-mode t)
  :config
  (setq super-save-auto-save-when-idle t
        auto-save-default nil))

(use-package savehist :ensure t  :defer t
  :init (savehist-mode t)
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (concat user-cache-directory "savehist")))

(use-package desktop :ensure t :defer t
  :config
  ;; Automatically save and restore sessions
  (make-directory (concat user-cache-directory "desktop") t)
  (setq desktop-dirname (concat user-cache-directory "desktop")
        desktop-base-file-name "emacs.desktop"
        desktop-base-lock-name "lock"
        desktop-path '(desktop-dirname)
        desktop-save t
        desktop-files-not-to-save "^$" ;reload tramp paths
        desktop-load-locked-desktop nil)
  (setq desktop-globals-to-save
        (append '((extended-command-history . 30)
                  (file-name-history        . 100)
                  (grep-history             . 30)
                  (minibuffer-history       . 50)
                  (query-replace-history    . 30)
                  (shell-command-history    . 50)
                  tags-file-name
                  register-alist))
        desktop-locals-to-save nil))

;; save recent files
(use-package recentf :ensure t :defer t
  :init (recentf-mode t)
  :config
  (setq recentf-save-file (concat user-cache-directory "recentf")
        recentf-max-saved-items 100
        recentf-max-menu-items 25))

;;;--------------------------------------------------
(provide 'config-basic)
