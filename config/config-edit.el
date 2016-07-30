;;; package --- summary

;;; Commentary: as a newbie in emacs world, I stolen a lot of codes from emacs vaterans

;;; Code:
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (set-fontset-font "fontset-default" 'gb18030' ("STHeiti" . "unicode-bmp"))
  (set-frame-font "Monaco 14")
  )

;; auto-complete mode
(use-package auto-complete
  :ensure t
  :config (global-auto-complete-mode t))
(diminish 'auto-complete-mode)

;; enable subword-mode
(global-subword-mode t)
(diminish 'subword-mode)

(setq-default indent-tabs-mode nil  ;; don't use tabs to indent
              tab-width 4           ;; but maintain correct appearance
              case-fold-search t    ;; case INsensitive search
              default-directory "~"
              fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

;; take care of the whitespace
(require 'whitespace)
(setq whitespace-style '(face trailing lines-tail
                              space-before-tab
                              indentation space-after-tab)
      whitespace-line-column 80)

;; nice things
(setq next-line-add-newlines nil  ;; don't add new lines when scrolling down
      require-final-newline t     ;; end files with a newline
      mouse-yank-at-point t       ;; yank at cursor, NOT at mouse position
      kill-whole-line t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode t)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t     ;; rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*") ;; don't muck with special buffers

;; activate it for all buffers
(setq-default save-place t)
(require 'saveplace)

;; saveplace remembers your location in a file when saving files
(setq save-place-file (local-file-name "cache/saveplace"))

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (local-file-name "cache/savehist"))
(savehist-mode t)

(require 'desktop)
(setq-default desktop-missing-file-warning nil
              desktop-load-locked-desktop t
              desktop-restore-eager 0
              desktop-path `(,(local-file-name "cache"))
              desktop-save t)
(desktop-save-mode t)

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
(desktop-read)

;; save recent files
(require 'recentf)
(setq recentf-save-file (local-file-name "cache/recentf")
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; diminish keeps the modeline tidy
;; (require 'diminish)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (local-file-name "cache/eshell"))

;; interface enhancement
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; error checking
;; (use-package flycheck
;;   :ensure flycheck
;;   :init (global-flycheck-mode)
;;   :diminish flycheck-mode)

;; completion
(use-package company
  :ensure t
  :init
  (progn
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-frontends '(company-pseudo-tooltip-frontend)))
  (defvar-local company-fci-mode-on-p nil)
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    ;; key bindings
    (let ((map company-active-map))
      (define-key map (kbd "C-/") 'company-search-candidates)
      (define-key map (kbd "C-M-/") 'company-filter-candidates)
      (define-key map (kbd "C-d") 'company-show-doc-buffer)
      (define-key map (kbd "C-j") 'company-select-next)
      (define-key map (kbd "C-k") 'company-select-previous)
      (define-key map (kbd "C-l") 'company-complete-selection))
    (use-package company-emoji
      :ensure t
      :init (company-emoji-init))
    (setq company-idle-delay 0.2))
  :diminish company-mode)

;; programming
(use-package aggressive-indent
  :ensure t
  :init
  (progn
    (global-aggressive-indent-mode 1)
    ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode) 
    ;; (add-to-list 'aggressive-indent-excluded-modes 'swift-mode)
    )
  :diminish aggressive-indent-mode
  )

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1)
  )

(use-package whitespace-cleanup-mode
  :ensure t
  :init
  (require 'whitespace-cleanup-mode)
  (global-whitespace-cleanup-mode)
  :diminish whitespace-cleanup-mode)

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
  :diminish smartparens-mode)

(use-package evil-nerd-commenter
  :ensure t
  :defer t
  :bind
  (("M-;" . evilnc-comment-or-uncomment-lines)
   ("C-c l" . evilnc-quick-comment-or-uncomment-to-the-line)
   ("C-c c" . evilnc-copy-and-comment-lines)
   ("C-c p" . evilnc-comment-or-uncomment-paragraphs))
  )

;; ---------------------------
;; file manager
;; ---------------------------
(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle))

;; (use-package helm-config
;;   :ensure helm
;;   :config
;;   (helm-mode 1)
;;   (helm-adaptive-mode 1)
;;   (helm-push-mark-mode 1))

(use-package helm
  :ensure t
  :init
  (progn
    (require 'helm-config)
    (setq helm-split-window-in-side-p t
          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-move-to-line-cycle-in-source t
          helm-ff-search-library-in-sexp t
          helm-ff-file-name-history-use-recentf t
          helm-echo-input-in-header-line t)
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    ;; (c) Emacs Prelude
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (helm-mode))
  :diminish helm-mode
  :bind (("C-c h" . helm-mini)
         ("C-c i" . helm-imenu)
         ("C-h a" . helm-apropos)
         ("C-x f" . helm-recentf)
         ("C-x b" . helm-for-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)))

(use-package dired
  :init
  (progn
    (use-package dired+)
    (use-package dired-details)
    (use-package dired-details+)
    (use-package dired-isearch)
    (use-package dired-single)

    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-recursive-copies (quote always))
    (setq dired-recursive-deletes (quote top))
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-dwim-target t)
    
    (add-hook 'dired-mode-hook
              (lambda ()
                (define-key dired-mode-map (kbd "<return>")
                  'dired-find-alternate-file) ; was dired-advertised-find-file
                (define-key dired-mode-map (kbd "^")
                  (lambda () (interactive) (find-alternate-file "..")))))))

;; sensible undo
(use-package undo-tree
  :ensure t
  :commands undo-tree-visualize
  :bind (("C-S-z" . undo-tree-redo)
         ("C-S-u" . undo-tree-visualize))
  :config (progn
            (global-undo-tree-mode)
            (setq undo-tree-visualizer-timestamps t
                  undo-tree-visualizer-diff t
                  undo-tree-auto-save-history t)

            (defadvice undo-tree-make-history-save-file-name
                (after undo-tree activate)
              (setq ad-return-value (concat ad-return-value ".gz")))

            (custom-set-variables
             '(undo-tree-history-directory-alist
               (quote (("." . "~/.emacs.d/undo/"))))))
  :diminish undo-tree-mode)

(use-package swiper
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  :ensure t
  :diminish swiper-mode)

(use-package ivy
  :init
  (ivy-mode 1)
  ;; show recently killed buffers when calling `ivy-switch-buffer'
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus))) ; default
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

  (define-key ivy-minibuffer-map (kbd "<C-tab>") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "<C-S-tab>") 'ivy-previous-line)
  :diminish ivy-mode)

;; (use-package hlinum
;;   :ensure t
;;   :config
;;   (hlinum-activate)
;;   (global-linum-mode t))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; editing
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package whole-line-or-region
  :ensure t
  :defer t
  :config
  (whole-line-or-region-mode t)
  :diminish whole-line-or-region-mode)

(use-package multiple-cursors
  :ensure t
  :defer t
  :init
  (require 'multiple-cursors)
  :diminish multiple-cursors-mode
  :config
  (progn
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
    ;; From active region to multiple cursors:
    (global-set-key (kbd "C-c C-r") 'set-rectangular-region-anchor)
    (global-set-key (kbd "C-c C-l") 'mc/edit-lines)
    (global-set-key (kbd "C-c C-e") 'mc/edit-ends-of-lines)
    (global-set-key (kbd "C-c C-a") 'mc/edit-beginnings-of-lines)
    ))

(use-package hungry-delete
  :ensure t
  :init (progn
          (global-hungry-delete-mode))
  :diminish hungry-delete-mode)

(use-package fix-word
  :ensure
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize))
  :diminish fix-word)


(use-package  which-key
  :ensure t
  :init
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :diminish which-key-mode)

(use-package sublimity
  :ensure t
  :config
  (sublimity-mode 1))

;; multiple-cursors-mode
;; (require-package 'multiple-cursors)
;; multiple-cursors
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
;; (global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
;; (global-set-key (kbd "C-c c c") 'mc/edit-lines)
;; (global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)
;; switch-window
;; outline-minor-mode
;; use C-u C-u C-s/r to trigger the flexible search action
;; set some compilation shortcuts
;; set smooth-scrolling
;; set stripe-buffer
;; (require 'stripe-buffer)
;; (use-package minimal
;;   :load-path "~/.emacs.d/lisp/minimal/"
;;   :bind "C-S-m")

;; use word-count
;; (require 'word-count)

;; (use-package eshell
;;   :init
;;   (defun eshell-new ()
;;     (interactive)
;;     (eshell t))
;;   :bind (("C-c m" . eshell)
;;          ("C-c M" . eshell-new))
;;   :config
;;   (setq eshell-directory-name (expand-file-name "eshell" dotemacs-cache-dir)))

(provide 'config-edit)

;;; config-edit.el ends here 

