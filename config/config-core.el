;; tells emacs not to load any packages before starting up
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

(require 'package)
(setq package-archives
      '(
        ;;("org"       . "http://orgmode.org/elpa/")
        ("org" . "http://elpa.emacs-china.org/org/")
        ;;("gnu"       . "http://elpa.gnu.org/packages/")
        ("gnu" . "http://elpa.emacs-china.org/gnu/")
        ;;("melpa"     . "https://melpa.org/packages/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ))
(package-initialize)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

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

(defconst user-cache-directory
  (file-name-as-directory (concat user-emacs-directory ".cache"))
  "My emacs storage area for persistent files.")
;; create the `user-cache-directory' if it doesn't exist
(make-directory user-cache-directory t)

;;;-----------------------------------------------------------------------------
;;; Back-up and Histroy
;;;-----------------------------------------------------------------------------
(let ((auto-save-dir (concat user-cache-directory "auto-save")))
  ;; Move backup file to `~/.emacs.d/.cache/auto-save
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t)))
  ;; Makesure auto-save directory exist
  (when (not (file-exists-p auto-save-dir))
    (make-directory auto-save-dir t)))
(setq create-lockfiles nil)

(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)

;; savehist keeps track of some history
(use-package savehist
  :ensure t
  :defer t
  :init (savehist-mode t)
  :config
  (setq savehist-additional-variables
        '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (concat user-cache-directory "savehist")))

(use-package desktop
  :ensure t
  :defer t
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
(use-package recentf
  :ensure t
  :defer t
  :init (recentf-mode t)
  :config
  (progn
    (setq recentf-save-file (concat user-cache-directory "recentf")
          recentf-max-saved-items 100
          recentf-max-menu-items 25)))


;; garbage collection
;; Increase the garbage collection threshold to decrease startup time
(setq gc-cons-threshold 100000000)
(eval-after-load 'minibuffer
  '(progn
     (lexical-let ((default-threshold gc-cons-threshold))
       (defun my/minibuffer-gc-setup-hook ()
         (setq gc-cons-threshold most-positive-fixnum))
       (add-hook 'minibuffer-setup-hook #'my/minibuffer-gc-setup-hook)
       ;; When exit, set back to default threshold
       (defun my/minibuffer-gc-exit-hook ()
         (setq gc-cons-threshold default-threshold))
       (add-hook 'minibuffer-exit-hook #'my/minibuffer-gc-exit-hook))))
(setq garbage-collection-messages t)

;; Time stamps
(setq
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
 time-stamp-format "Last modified on %04y-%02m-%02d %02H:%02M:%02S (%U)")
(add-hook 'write-file-hooks 'time-stamp)


;;; os setting
(let ((is-mac (string-equal system-type "darwin")))
  (when is-mac
    ;; make fonts look better with anti-aliasing
    (setq mac-allow-anti-aliasing t)
    ;; delete files by moving them to the trash
    (setq delete-by-moving-to-trash t)
    (setq trash-directory "~/.Trash")

    ;; Don't make new frames when opening a new file with Emacs
    (setq ns-pop-up-frames nil)

    ;; non-lion fullscreen
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
    (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
    ))

(provide 'config-core.el)
;;; config-package.el ends here
