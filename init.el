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

(defconst package-mirror "emacs-china"
  "Package mirror used.")

(defconst user-cache-directory
  (file-name-as-directory (concat user-emacs-directory ".cache"))
  "My emacs storage area for persistent files.")
(make-directory user-cache-directory t)

(defconst is-mac (string-equal system-type "darwin")
  "Whether is macos or not.")

(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

(when load-file-name
  (defconst base-path (file-name-directory load-file-name)))

(setq custom-file (concat base-path "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(cond
 ((string-equal package-mirror "emacs-china")
  (setq package-archives
		'(("org" . "http://elpa.emacs-china.org/org/")
		  ("gnu" . "http://elpa.emacs-china.org/gnu/")
		  ("melpa" . "http://elpa.emacs-china.org/melpa/"))))
 ((string-equal package-mirror "original")
  (setq package-archives
		'(("org"       . "http://orgmode.org/elpa/")
		  ("gnu"       . "http://elpa.gnu.org/packages/")
		  ("melpa"     . "https://melpa.org/packages/")))))

(package-initialize nil)
(when (not package-archive-contents) (package-refresh-contents))

(unless (package-installed-p 'org)
 (package-refresh-contents)
 (package-install 'org))

(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

(setq use-package-verbose t)

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

;;
;; os setting
;;

(when is-mac
  (setq
   mac-allow-anti-aliasing t
   delete-by-moving-to-trash t
   trash-directory "~/.Trash"
   ns-pop-up-frames nil
   ns-use-native-fullscreen nil)
  ;; Set modifier keys
  (setq
   mac-option-modifier 'meta
   mac-command-modifier 'super
   mac-function-modifier 'hyper
   mac-right-option-modifier 'none))

;; Time stamps
(setq
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
 time-stamp-format "Last modified on %04y-%02m-%02d %02H:%02M:%02S (%U)")
(add-hook 'write-file-hooks 'time-stamp)

(setq
 inhibit-startup-screen t
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
 )

(put 'dired-find-alternate-file 'disabled nil)

(show-paren-mode t)
;; (setq show-paren-style 'expression)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)
(diminish 'abbrev-mode)
(setq abbrev-file-name (concat user-cache-directory "emacs_abbre.el"))

(fset 'yes-or-no-p 'y-or-n-p)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; visual line
(global-visual-line-mode 1)
(diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)

;; enable subword-mode
(global-subword-mode t)
(diminish 'subword-mode)
;; delete the selection with a keypress
(delete-selection-mode t)

(setq-default indent-tabs-mode nil  ;; don't use tabs to indent
              tab-width 4           ;; but maintain correct appearance
              case-fold-search t    ;; case INsensitive search
              default-directory "~"
              fill-column 80

              next-line-add-newlines nil  ;; don't add new lines when scrolling down
              require-final-newline t     ;; end files with a newline
              mouse-yank-at-point t       ;; yank at cursor, NOT at mouse position
              kill-whole-line t)

;;(setq display-time-format "%a %b %d | %H:%M |")
;;(display-time-mode)

(add-hook 'after-init-hook
          (lambda ()
            (fringe-mode '(8 . 2)) ; Make fringe look good with git-gutter-fringe+
			;; Set font
            (set-face-attribute 'default nil
								:family "Source Code Pro"
								:height 120
								:weight 'normal
								:width 'normal)))

(setq shell-file-name "/usr/local/bin/bash")
(setq explicit-bash-args '("--login" "--init-file" "$HOME/.bash_profile" "-i"))

(use-package exec-path-from-shell :ensure t :defer t
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


(use-package better-defaults :ensure t :defer t)

(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name user-emacs-directory))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))

(defun config-file (suffix)
  (load-file (local-file-name (concat "config/config-" suffix ".el"))))

(setq configs '(
                "navigation"
                "appearance"
                "evil"
                "keybindings"
                "defuns"
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

(use-package super-save :ensure t :defer t
  :init
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))

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

;; save recent files
(use-package recentf :ensure t
  :init (recentf-mode t)
  :config
  (setq recentf-save-file (concat user-cache-directory "recentf")
		recentf-max-saved-items 100
		recentf-max-menu-items 25))

;;(benchmark-init/show-durations-tree)

(provide 'init)
;;; init.el ends here
