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
(use-package whitespace
  :defer t
  :init
  (setq whitespace-style '(face
                           trailing
                           lines-tail
                           space-before-tab
                           indentation
                           space-after-tab)
        whitespace-line-column 80))

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

(use-package saveplace
  :defer t
  :init
  (progn
    (setq-default save-place t)
    ;; saveplace remembers your location in a file when saving files
    (setq save-place-file (local-file-name "cache/saveplace"))))


(defun lix--dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; editing
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-mode
  :init
  (whole-line-or-region-mode t))

(use-package multiple-cursors
  :ensure t
  :init (require 'multiple-cursors)
  :diminish multiple-cursors-mode
  :bind
  ("C-<" . mc/mark-previous-like-this)
  ("C->" .  mc/mark-next-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ;; From active region to multiple cursors:
  ("C-c C-r" . set-rectangular-region-anchor)
  ("C-c C-l" . mc/edit-lines)
  ("C-c C-e" . mc/edit-ends-of-lines)
  ("C-c C-a" . mc/edit-beginnings-of-lines))
(use-package phi-search
  :ensure t)
(define-key mc/keymap (kbd "C-s") 'phi-search)
(define-key mc/keymap (kbd "C-r") 'phi-search-backward)

(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode)
  :diminish hungry-delete-mode)

(use-package fix-word
  :ensure t
  :diminish fix-word
  :bind
  ("M-u" . fix-word-upcase)
  ("M-l" . fix-word-downcase)
  ("M-c" . fix-word-capitalize))

(use-package move-text
  :ensure t
  :bind
  ("M-n" . move-text-down)
  ("M-p" . move-text-up))

;; (use-package dash
;;   :ensure t
;;   :config
;;   (use-package dash-functional
;;     :ensure t)
;;   (dash-enable-font-lock))
;; (use-package s
;;   :ensure t)
;; (use-package origami
;;   :ensure t
;;   :init (global-origami-mode t))

;; dash-at-point
;; (use-package dash-at-point
;;   :ensure t
;;   :defer 3
;;   :bind (("C-c d" . dash-at-point)
;;          ("C-c e" . dash-at-point-with-docset)))

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

(use-package crux
  :ensure t
  :bind
  ("C-c o" . crux-open-with)
  ("C-c f" . crux-recentf-find-file)
  ("C-a" . crux-move-beginning-of-line)
  :init
  (setq crux-shell "/bin/zsh")
  (defalias 'zsh 'crux-visit-term-buffer))

(use-package phi-rectangle
  :ensure t
  :bind
  ("C-c u C-r" . phi-rectangle-set-mark-command))

;; *scratch* buffer
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

(setq initial-major-mode 'markdown-mode)
(setq initial-scratch-message "\
# This buffer is for notes you don't want to save, and for Markdown.
# If you want to create a file, visit that file with C-x C-f,
# then enter the text in that file's own buffer.")



(provide 'config-edit)
;;; config-edit.el ends here
