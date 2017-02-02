;; General
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

;; nice things
(setq next-line-add-newlines nil  ;; don't add new lines when scrolling down
      require-final-newline t     ;; end files with a newline
      mouse-yank-at-point t       ;; yank at cursor, NOT at mouse position
      kill-whole-line t)

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

(use-package general
  :ensure t
  :init
  (general-create-definer cpm-leader1
                          :states '(normal insert visual motion emacs)
                          :keymaps 'global
                          :prefix "SPC"
                          :non-normal-prefix "H-SPC"))

;;; search
;; (use-package avy
;;   :ensure t
;;   :commands (avy-goto-char))
(use-package ace-jump-mode
  :ensure t
  :commands (ace-jump-word-mode
             ace-jump-char-mode
             ace-jump-line-mode))

;;; navigate text
(use-package move-text
  :ensure t
  :bind
  ("H-n" . move-text-down)
  ("H-p" . move-text-up))

;;; navigate window
(use-package window-numbering
  :ensure t
  :config
  (progn
    (defun window-numbering-install-mode-line (&optional position)
      "Do nothing, the display is handled by the powerline.")
    (setq window-numbering-auto-assign-0-to-minibuffer nil)
    (cpm-leader1
     "0" 'select-window-0
     "1" 'select-window-1
     "2" 'select-window-2
     "3" 'select-window-3
     "4" 'select-window-4
     "5" 'select-window-5)
    ;; "6" 'select-window-6
    ;; "7" 'select-window-7
    ;; "8" 'select-window-8
    ;; "9" 'select-window-9)
    (window-numbering-mode 1)

    (defun spacemacs//window-numbering-assign (windows)
      "Custom number assignment for special buffers."
      (mapc (lambda (w)
              (when (and (boundp 'neo-global--window)
                         (eq w neo-global--window))
                (window-numbering-assign w 0)))
            windows))
    (add-hook 'window-numbering-before-hook 'spacemacs//window-numbering-assign)
    (add-hook 'neo-after-create-hook '(lambda (w) (window-numbering-update))))
  )

;; (use-package windmove
;;   :defer t
;;   :config
;;   (defun split-window-right-and-focus ()
;;     "Split the window horizontally and focus the new window."
;;     (interactive)
;;     (split-window-right)
;;     (windmove-right))
;;   (defun split-window-below-and-focus ()
;;     "Split the window vertically and focus the new window."
;;     (interactive)
;;     (split-window-below)
;;     (windmove-down))
;;   ;; add edit mode keybindings
;;   (global-set-key (kbd "<H-up>")     'windmove-up)
;;   (global-set-key (kbd "<H-down>")   'windmove-down)
;;   (global-set-key (kbd "<H-left>")   'windmove-left)
;;   (global-set-key (kbd "<H-right>")  'windmove-right)
;;   )

(use-package dired-details
  :ensure t
  :config
  (progn
    (require 'dired)
    (setq-default dired-details-hidden-string "--- ")
    (setq dired-dwim-target t)
    ;; Reload dired after making changes
    (--each '(dired-do-rename
              dired-do-copy
              dired-create-directory
              wdired-abort-changes)
      (eval `(defadvice ,it (after revert-buffer activate)
               (revert-buffer))))
    ))

;;; project
(use-package project-explorer
  :ensure t
  :defer t)
;; (add-hook 'project-explorer-mode-hook (lambda ()
;;                                         (unbind-key "M-o" project-explorer-mode-map)))

(use-package aggressive-indent
  :ensure t
  :defer t
  :init
  (global-aggressive-indent-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :init
  (global-whitespace-cleanup-mode))

(use-package semantic
  :defer t
  :config
  (progn
    (add-to-list 'semantic-default-submodes
                 'global-semantic-stickyfunc-mode)
    (add-to-list 'semantic-default-submodes
                 'global-semantic-idle-summary-mode)))

(use-package undo-tree
  :ensure t
  :commands (undo-tree-undo undo-tree-visualize)
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (let ((undo-dir (concat user-cache-directory "undo")))
      (setq undo-tree-history-directory-alist '((".*" . ,undo-dir))))))

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
  :diminish multiple-cursors-mode
  :config
  (progn
    (use-package phi-search
      :ensure t)
    (define-key mc/keymap (kbd "C-s") 'phi-search)
    (define-key mc/keymap (kbd "C-r") 'phi-search-backward)))

(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode)
  :diminish hungry-delete-mode)

(use-package fix-word
  :ensure t
  :defer t
  :diminish fix-word
  :bind
  ("M-u" . fix-word-upcase)
  ("M-l" . fix-word-downcase)
  ("M-c" . fix-word-capitalize))

(use-package crux
  :ensure t
  :bind ("C-a" . crux-move-beginning-of-line)
  :config
  (setq crux-shell "/bin/zsh")
  (defalias 'zsh 'crux-visit-term-buffer))

(use-package phi-rectangle :ensure t)

(use-package reveal-in-osx-finder
  :ensure t
  :defer t
  :bind ("C-c C-f" . reveal-in-osx-finder))

(provide 'config-navigation)
;;; config-navigation.el ends here
