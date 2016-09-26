;;; package --- summary

;;; Commentary: as a newbie in emacs world, I stolen a lot of codes from emacs vaterans

;;; Code:

;; enable subword-mode
(global-subword-mode t)
(diminish 'subword-mode)
;; delete the selection with a keypress
(delete-selection-mode t)

(setq-default indent-tabs-mode nil  ;; don't use tabs to indent
              tab-width 4           ;; but maintain correct appearance
              case-fold-search t    ;; case INsensitive search
              default-directory "~"
              fill-column 80)

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

;; shell
(setq shell-file-name "/bin/zsh")
(global-set-key (kbd "C-c z") 'shell) 
(global-set-key (kbd "<f10>") 'rename-buffer)

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :init
  (global-aggressive-indent-mode 1)
  )

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config (progn
            (yas-reload-all)
            (yas-global-mode t)
            ;; (add-hook 'prog-mode-hook #'yas-minor-mode)
            ))

(use-package whitespace-cleanup-mode
  :ensure t
  :init (progn
          (global-whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

;; (use-package evil-nerd-commenter
;;   :ensure t
;;   :defer t
;;   :bind
;;   (
;;    ("M-;" . evilnc-comment-or-uncomment-lines)
;;    ("C-c l" . evilnc-quick-comment-or-uncomment-to-the-line)
;;    ("C-c c" . evilnc-copy-and-comment-lines)
;;    ("C-c p" . evilnc-comment-or-uncomment-paragraphs)
;;    )
;;   )

;; paredit
;; (use-package paredit
;;   :ensure t
;;   :commands (enable-paredit-mode)
;;   :diminish paredit-mode
;;   :init (progn
;;           (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;;           (add-hook 'clojure-mode-hook 'enable-paredit-mode)))

;; sensible undo
(use-package undo-tree
  :ensure t
  :commands undo-tree-visualize
  :bind (("C-S-z" . undo-tree-redo)
         ("C-S-u" . undo-tree-visualize))
  :config
  (progn
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

;; (use-package multiple-cursors
;;   :ensure t
;;   :defer t
;;   :init
;;   (require 'multiple-cursors)
;;   :diminish multiple-cursors-mode
;;   :config
;;   (progn
;;     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;     (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;     (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;;     ;; From active region to multiple cursors:
;;     (global-set-key (kbd "C-c C-r") 'set-rectangular-region-anchor)
;;     (global-set-key (kbd "C-c C-l") 'mc/edit-lines)
;;     (global-set-key (kbd "C-c C-e") 'mc/edit-ends-of-lines)
;;     (global-set-key (kbd "C-c C-a") 'mc/edit-beginnings-of-lines)
;;     ))

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

;; (use-package sublimity
;;   :ensure t
;;   :config
;;   (sublimity-mode 1))

;; (use-package smart-shift
;;   :ensure t
;;   :init (global-smart-shift-mode 1))

(use-package benchmark-init
  :ensure t
  :init
  (benchmark-init/activate))

(provide 'config-edit)

;;; config-edit.el ends here 

