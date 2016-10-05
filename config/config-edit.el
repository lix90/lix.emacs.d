;;; edit --- configuration for editing configuration:
;;; Commentary:
;;; as a newbie in Emacs world, I stolen a lot of codes from Emacs vaterans
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

;; -----------------------------------
;; saveplace
;; -----------------------------------
(require 'saveplace)
(setq-default save-place t)
;; saveplace remembers your location in a file when saving files
(setq save-place-file (local-file-name "cache/saveplace"))

;; -----------------------------------
;; switch buffer
;; -----------------------------------
;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; ;; ediff - don't start another frame
;; (require 'ediff)
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)


;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :init
  (global-aggressive-indent-mode 1)
  )

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

;; sensible undo
(use-package undo-tree
  :ensure t
  :commands undo-tree-visualize
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t
          undo-tree-auto-save-history t)
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
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode t))

(use-package multiple-cursors
  :ensure t
  :init (require 'multiple-cursors)
  :diminish multiple-cursors-mode
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" .  mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ;; From active region to multiple cursors:
         ("C-c C-r" . set-rectangular-region-anchor)
         ("C-c C-l" . mc/edit-lines)
         ("C-c C-e" . mc/edit-ends-of-lines)
         ("C-c C-a" . mc/edit-beginnings-of-lines)))

(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode)
  :diminish hungry-delete-mode)

(use-package fix-word
  :ensure t
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize))
  :diminish fix-word)

(use-package  which-key
  :ensure t
  :init
  (progn
    (which-key-mode)
    (which-key-setup-side-window-bottom))
  :diminish which-key-mode)

;; -----------------------------------
;; benchmark
;; -----------------------------------
(use-package benchmark-init
  :ensure t
  :init
  (benchmark-init/activate))

;; -----------------------------------
;; text folding minor mode
;; -----------------------------------
(use-package origami
  :ensure t
  :init (global-origami-mode))

;; -----------------------------------
;; moving text
;; -----------------------------------
(use-package move-text
  :ensure t
  :bind (("M-n" . move-text-down)
         ("M-p" . move-text-up)))

(provide 'config-edit)

;;; config-edit.el ends here

