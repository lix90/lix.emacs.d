;; enable subword-mode
(global-subword-mode t)
(diminish 'subword-mode)
;; ---------------------
;; interface enhancement
;; ---------------------
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; ---------------------
;; error checking
;; ---------------------
;; (use-package flycheck
;;   :ensure flycheck
;;   :init (global-flycheck-mode)
;;   :diminish flycheck-mode)

;; ---------------------
;; completion
;; ---------------------
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    (use-package company-emoji
      :ensure t
      :init (company-emoji-init))
    (setq company-idle-delay 0.2))
  :diminish company-mode)

;; -----------------------
;; programming
;; -----------------------
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

(use-package helm-config
  :ensure helm
  :config
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-push-mark-mode 1))

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

(use-package undo-tree
  :init (undo-tree-mode t)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)
         ("s-u" . undo-tree-visualize))
  :ensure t
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

(use-package hlinum
  :ensure t
  :config
  (hlinum-activate)
  (global-linum-mode t))

(use-package rainbow-delimiters
  :ensure t
  :init
  (rainbow-delimiters-mode-enable))

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
  :pin "melpa")

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
;; use minimal mode
;; (require 'minimal)
;; use word-count
;; (require 'word-count)

(use-package eshell
  :init
  (defun eshell-new ()
    (interactive)
    (eshell t))
  :bind (("C-c m" . eshell)
         ("C-c M" . eshell-new))
  :config
  (setq eshell-directory-name (expand-file-name "eshell" dotemacs-cache-dir)))

(provide 'config-edit)

