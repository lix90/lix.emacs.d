;;; init --- My Emacs initialization file:

;; This file has been updated according to many awesome dot.emacs.d

;; Author: Xiang, Li <alexiangli@outlook.com>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl)

(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

(when load-file-name
  (defconst base-path (file-name-directory load-file-name)))

(setq custom-file (concat base-path "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq package-archives
      '(
        ;;("org"       . "http://orgmode.org/elpa/")
        ("org" . "http://elpa.emacs-china.org/org/")
        ;;("gnu"       . "http://elpa.gnu.org/packages/")
        ("gnu" . "http://elpa.emacs-china.org/gnu/")
        ;;("melpa"     . "https://melpa.org/packages/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ))

(package-initialize nil)
(when (not package-archive-contents) (package-refresh-contents))

(unless (package-installed-p 'org)
 (package-refresh-contents)
 (package-install 'org))

(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

(defvar use-package-verbose t)

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package benchmark-init :ensure t
  :init (require 'benchmark-init))
(add-hook
 'benchmark-init/tree-mode-hook
 '(lambda ()
    (local-set-key "i" '(lambda () (interactive) (find-file user-init-file)))
    (local-set-key "s" '(lambda () (interactive) (switch-to-buffer "*scratch*")))
    (local-set-key "t" 'counsel-load-theme)
    (local-set-key "f" 'counsel-set-font)
    (local-set-key "a" 'org-agenda)
    (local-set-key "p" 'projectile-switch-project)))

;;------------------------------------------------------------------------------
;; Basic settings
;;------------------------------------------------------------------------------

(defconst user-cache-directory
  (file-name-as-directory (concat user-emacs-directory ".cache"))
  "My emacs storage area for persistent files.")
(make-directory user-cache-directory t)

;;
;; os setting
;;

(setq is-mac (string-equal system-type "darwin"))
(when is-mac
  (setq mac-allow-anti-aliasing t)
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  (setq ns-pop-up-frames nil)
  (setq ns-use-native-fullscreen nil)

  ;; Set modifier keys
  (setq mac-option-modifier 'meta) ;; Bind meta to ALT
  (setq mac-command-modifier 'super) ;; Bind apple/command to super if you want
  (setq mac-function-modifier 'hyper) ;; Bind function key to hyper if you want
  (setq mac-right-option-modifier 'none) ;; unbind right key for accented input

  ;; Make forward delete work
  (global-set-key (kbd "<H-backspace>") 'delete-forward-char)

  ;; Keybindings
  (global-set-key (kbd "s-=") 'scale-up-font)
  (global-set-key (kbd "s--") 'scale-down-font)
  (global-set-key (kbd "s-0") 'reset-font-size)
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-c") 'evil-yank)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "s-s")
                  (lambda ()
                    (interactive)
                    (call-interactively (key-binding "\C-x\C-s"))))
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
  ;; Emacs sometimes registers C-s-f as this weird keycode
  (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen))


;;;-----------------------------------------------------------------------------
;;; Back-up and Histroy
;;;-----------------------------------------------------------------------------

;; (let ((auto-save-dir (concat user-cache-directory "auto-save")))
;;   ;; Move backup file to `~/.emacs.d/.cache/auto-save
;;   (setq auto-save-file-name-transforms
;;         `((".*" ,auto-save-dir t)))
;;   ;; Makesure auto-save directory exist
;;   (when (not (file-exists-p auto-save-dir))
;;     (make-directory auto-save-dir t)))
(setq create-lockfiles nil)
(use-package super-save :ensure t :defer t
  :init
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))

;; (defun full-auto-save ()
;;   (interactive)
;;   (save-excursion
;;     (dolist (buf (buffer-list))
;;       (set-buffer buf)
;;       (if (and (buffer-file-name) (buffer-modified-p))
;;           (basic-save-buffer)))))
;; (add-hook 'auto-save-hook 'full-auto-save)

;; savehist keeps track of some history
(use-package savehist :ensure t  :defer t
  :init (savehist-mode t)
  :config
  (setq savehist-additional-variables
        '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (concat user-cache-directory "savehist")))

(use-package desktop :ensure t :defer t
  :init (desktop-save-mode 0)
  :config
  (progn
    ;; Automatically save and restore sessions
    (make-directory (concat user-cache-directory "desktop") t)

    (setq desktop-dirname (concat user-cache-directory "desktop")
          desktop-base-file-name "emacs.desktop"
          desktop-base-lock-name "lock"
          desktop-path (list desktop-dirname)
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
          desktop-locals-to-save nil)

    (defun my-desktop ()
      "Load the desktop and enable autosaving"
      (interactive)
      (let ((desktop-load-locked-desktop "ask"))
        (desktop-read)
        (desktop-save-mode 1)))
    )
  )

;; save recent files
(use-package recentf :ensure t :defer t
  :init (recentf-mode t)
  :config
  (progn
    (setq recentf-save-file (concat user-cache-directory "recentf")
          recentf-max-saved-items 100
          recentf-max-menu-items 25)))

;; Time stamps
(setq
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
 time-stamp-format "Last modified on %04y-%02m-%02d %02H:%02M:%02S (%U)")
(add-hook 'write-file-hooks 'time-stamp)

(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name user-emacs-directory))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))

(defun config-file (suffix)
  (load-file (local-file-name (concat "config/config-" suffix ".el"))))

(setq load-prefer-newer t)

(setq configs '(
                "navigation"
                "appearance"
                "evil"
                "keybindings"
                "defuns"
                "private"
                "shell"
                "programming"
                "version-control"
                "data-science"
                "writing"
                "languages"
                "utilities"
                ))

(dolist (suffix configs)
  (config-file suffix))

;; (use-package config-navigation :load-path "config")
;; (use-package config-appearance :load-path "config")
;; (use-package config-evil :load-path "config")
;; (use-package config-keybindings :load-path "config")
;; (use-package config-private :load-path "config")
;; (use-package config-shell :load-path "config")
;; (use-package config-programming :load-path "config")
;; (use-package config-version-control :load-path "config")
;; (use-package config-data-science :load-path "config")
;; (use-package config-web-development :load-path "config")
;; (use-package config-writing :load-path "config")
;; (use-package config-languages :load-path "config")
;; (use-package config-utilities :load-path "config")

;;------------------------------------------------------------------------------
;; Basic settings
;;------------------------------------------------------------------------------

(setq buffer-file-coding-system 'utf-8-unix)
(setq abbrev-file-name (concat user-cache-directory "abbrev_defs"))
(setq message-log-max 10000)
(fset 'yes-or-no-p 'y-or-n-p)

(setq
 inhibit-startup-screen t
 initial-scratch-message nil
 ;; nice scrolling
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1
 cursor-in-non-selected-windows nil
 use-dialog-box nil
 ring-bell-function 'ignore)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; visual line
(global-visual-line-mode 1)
(diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)

;;(setq display-time-format "%a %b %d | %H:%M |")
;;(display-time-mode)

(setq shell-file-name "/usr/local/bin/bash")
(setq explicit-bash-args '("--login" "--init-file" "$HOME/.bash_profile" "-i"))

(use-package exec-path-from-shell :ensure t :defer 10
  :if (memq window-system '(mac ns))
  :init
  ;; Solve warning of setting locale in ESS
  ;; from: https://stat.ethz.ch/pipermail/r-sig-mac/2015-October/011701.html
  (exec-path-from-shell-copy-env "LC_ALL")
  (exec-path-from-shell-copy-env "LANG"))

;;
;; Set Path
;;
(setenv "PATH" (concat "/Users/lix/anaconda3/bin/:/usr/local/bin:" (getenv "PATH")))
(setenv "WORKON_HOME" "/Users/lix/anaconda3/envs/")
(setq exec-path (append '(
						  "/Users/lix/anaconda3/bin/"
						  "/usr/local/bin"
						  )
						exec-path))

;;------------------------------------------------------------------------------

(use-package popup :ensure t :defer t)
(use-package popwin :ensure t
  :demand
  :config (popwin-mode 1)
  (setq popwin:close-popup-window-timer-interval 0.1)
  (setq popwin:close-popup-window-timer nil)
  (defun popwin:flycheck-errors ()
    (interactive)
    (when (get-buffer "*Flycheck errors*") (popwin:popup-buffer "*Flycheck errors*")))
  (defun popwin:compilation ()
    (interactive)
    (when (get-buffer "*compilation*")
      (if (get-buffer-window "*compilation*")
          (delete-window (get-buffer-window "*compilation*"))
        (popwin:popup-buffer "*compilation*" :noselect t :stick t :tail t))))
  :bind ("C-x m" . popwin:messages)
  ("C-x e" . popwin:flycheck-errors)
  ("C-x c" . popwin:compilation))

(use-package nameless :ensure t :defer t
  :config (bind-keys :map nameless-mode-map ("C-c C-c" . nameless-insert-name)))

(use-package flycheck :ensure t :disabled t
  :config (global-flycheck-mode)
  (setq flycheck-javascript-standard-executable "standard")
  (setq flycheck-javascript-eslint-executable "eslint")
  (setq flycheck-eslintrc ".eslintrc.json")
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (bind-keys :map flycheck-mode-map
             ("C-c C-e" . flycheck-list-errors)
             ("C-c C-n" . flycheck-next-error)
             ("C-c C-p" . flycheck-previous-error))
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-standard 'rjsx-mode)
  ;;:bind ("M-}" . flycheck-mode)
  )


;;------------------------------------------------------------------------------
;; Writing
;;------------------------------------------------------------------------------

(use-package flyspell-popup :ensure t :defer t :after flyspell
  :config (bind-keys :map flyspell-mode-map ("±" . flyspell-popup-correct)))

(use-package flyspell :ensure t
  :init (defun flyspell-toggle ()
          (interactive)
          (if flyspell-mode (flyspell-mode-off) (flyspell-mode)))
  :config
  (setq ispell-dictionary "english")
  (dolist (hook '(text-mode-hook)) (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook)) (add-hook hook (lambda () (flyspell-mode -1))))
  (advice-add 'flyspell-mode-on :before 'flyspell-buffer)
  :bind ("M-{" . flyspell-toggle))

;;(benchmark-init/show-durations-tree)

(provide 'init)
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
