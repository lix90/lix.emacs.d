;; basic editing settings
(setq-default create-lockfiles nil
              buffer-file-coding-system 'utf-8-unix
              make-backup-files nil
              ;;indent-tabs-mode nil in better-default alrealy
              tab-width 4
              case-fold-search t
              default-directory "~"
              fill-column 80
              next-line-add-newlines nil
              require-final-newline t
              ;;mouse-yank-at-point t ;; alrealy in better-default
              kill-whole-line t
              indent-tabs-mode nil)

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places"))

(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(when is-mac
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash"
        ns-pop-up-frames nil
        ns-use-native-fullscreen nil)
  ;; Set modifier keys
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-function-modifier 'hyper
        mac-right-option-modifier 'none))

(global-subword-mode +1)
(delete-selection-mode +1)

;; hippie-expand
(setq hippie-expand-try-function-list
      '(try-expand-debbrev
        try-expand-debbrev-all-buffers
        try-expand-debbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;;--------------------
;; Learn abbrev
;;;--------------------
;; C-x a abbrev key map
;; C-x ' expand abbrev
;; You can use abbrev for:
;; * Long English words, e.g. “comm” for “communication”.
;; * Programing language function templates.
;; * emoji, such as “omg” for
;; * math symbols such as “ra” for →
;; * Templates, such as license header.
;; * Address, url, telephone number, company name, etc.
(dolist (hook (list
               prog-mode-hook
               text-mode-hook
               ))
  (add-hook 'hook #'abbrev-mode))
(setq save-abbrevs 'silently)
(setq abbrev-file-name
      (concat user-emacs-directory "emacs_abbre.el"))

;; Time stamps
(use-package time-stamp :ensure t :defer t
  :config
  (setq time-stamp-active t          ; do enable time-stamps
        time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
        time-stamp-format "Last modified on %04y-%02m-%02d %02H:%02M:%02S (%U)")
  (add-hook 'write-file-hooks #'time-stamp))

;;(setq display-time-format "%a %b %d | %H:%M |")
;;(display-time-mode)

;; 当idle或者光标失焦（lose focus）时，自动保存文件
(use-package super-save :ensure t :defer t
  :init
  (add-hook 'after-init-hook #'super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t
        auto-save-default nil))

;; 保存历史
(use-package savehist :ensure t  :defer t
  :init (add-hook 'after-init-hook #'savehist-mode)
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (concat user-emacs-directory "savehist")))

;; 保存桌面
(use-package desktop :ensure t :defer t :if (display-graphic-p)
  :config
  ;; Automatically save and restore sessions
  (make-directory (concat user-emacs-directory "desktop") t)
  (setq desktop-dirname (concat user-emacs-directory "desktop")
        desktop-base-file-name "emacs.desktop"
        desktop-base-lock-name "lock"
        desktop-path `(,desktop-dirname)
        desktop-save t
        desktop-files-not-to-save "^$"
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
  )

;; save recent files
(use-package recentf :ensure t :defer t
  :init (add-hook 'after-init-hook #'recentf-mode)
  :config
  (setq recentf-save-file (concat user-emacs-directory "recentf")
        recentf-max-saved-items 100
        recentf-max-menu-items 25))

;;;--------------------
;; anzu tutorial
;;;--------------------
;; 1. query-replace 查询、代替
;; 2. enter
;; 3. y -> act,
;;    n -> skip,
;;    Y -> act all,
;;    N -> skip all,
;;    , -> act but not next
(use-package anzu :ensure t :defer t
  :init (add-hook 'prog-mode-hook #'anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;; 智能mark选区
(use-package expand-region :ensure t :defer t
  :bind ("C-=" . er/expand-region)
  :init
  (bind-keys :prefix-map lix/expand-region-map
             :prefix "M-s e"
             :prefix-docstring "Expand Region"
             ("f" . er/mark-defun)
             ("c" . er/mark-comment)
             ("e" . er/mark-email)
             ("s" . er/mark-sentence)
             ("p" . er/mark-paragraph)))

;; learn iedit
;; 用于批量修改匹配的symbol或者word或者region
;; 基本用法：
;; C-;/M-s ; 开启或关闭iedit-mode
;; 在iedit-mode开启时，通过C-' 可以仅仅显示选中词出现的段落，
;; 隐藏其他段落
(use-package iedit :ensure t :defer t
  :init
  (bind-key* "C-;" 'iedit-mode))

;; 使用goto-last-change前往上一次修改的位置
(use-package goto-last-change :ensure t
  :bind (("M-g l" . goto-last-change)))

;; 需安装the_silver_searcher
;; gentoo linux: `emerge -avt the_silver_searcher`
;; macos: `brew install the_silver_searcher`
;; 可以与projectile进行绑定，在项目中搜索
(use-package ag :ensure t :defer t
  :init
  (bind-keys :prefix-map lix/ag-search
             :prefix "M-s f"
             :prefix-docstring "Ag Search"
             ("p" . ag-project)
             ("f" . ag-files)
             ("F" . ag-project-files)))

;; 快速导航，移动光标至指定行，字符，词等
(use-package avy :ensure t :defer t
  :init
  (bind-keys :prefix-map lix/avy-prefix-map
             :prefix "M-s a"
             :prefix-docstring "Avy Goto"
             ("l" . avy-goto-line)
             ("c" . avy-goto-char)
             ("C" . avy-goto-char-2)
             ("w" . avy-goto-word-0)
             ("W" . avy-goto-word-1)))

;; 基于`expand-region`增加/变更/删除pairs
;; add
;; change
;; delete
(use-package embrace :ensure t
  :bind ("C-," . embrace-commander))

(use-package crux :ensure t :defer t
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-S-k" . crux-kill-whole-line))
  :commands (crux-smart-open-line
             crux-smart-open-line-above
             crux-kill-whole-line)
  :init
  (progn
    (bind-keys :prefix-map lix/manipulate-line
               :prefix "C-o"
               :prefix-docstring "Open line"
               ("o" . open-line)
               ("j" . join-line)
               ("s" . split-line)
               ("d" . crux-smart-open-line)
               ("a" . crux-smart-open-line-above))
    (bind-keys :prefix-map lix/crux-helper
               :prefix "C-c c"
               :prefix-docstring "Crux Helper Functions"
               ("K" . crux-kill-other-buffers)
               ("n" . crux-create-scratch-buffer)
               ("b" . crux-find-shell-init-file)
               ("e" . crux-find-user-init-file)
               ("r" . crux-rename-file-or-buffer)
               ("C" . crux-cleanup-buffer-or-region))))

;; 饥饿删除模式
(use-package hungry-delete :ensure t :defer t
  :init
  (global-hungry-delete-mode t))

;; 矩形选择
(use-package phi-rectangle :ensure t :defer t
  :bind (("C-x s" . phi-rectangle-set-mark-command)))

;; 当未选中区域时，仍然可以对行进行操作
(use-package whole-line-or-region :ensure t :defer t
  :bind (("s-c" . whole-line-or-region-copy-region-as-kill)
         ("s-v" . whole-line-or-region-yank)
         ("s-x" . whole-line-or-region-kill-region)
         ("C-." . whole-line-or-region-comment-dwim))
  :config
  (dolist (hook '(prog-mode-hook
                  text-mode-hook
                  markdown-mode-hook))
    (add-hook 'hook #'whole-line-or-region-mode)))

;; 移动文本
(use-package move-text :ensure t :defer t
  :bind (("C-S-n" . move-text-down)
         ("C-S-p" . move-text-up)))

;; 编程大小写
(use-package fix-word :ensure t :defer t)

(use-package whitespace :ensure t :defer t
  :config
  (setq whitespace-style
        '(face
          trailing
          lines-tail
          space-before-tab
          indentation
          space-after-tab))
  (setq whitespace-line-column 80))

(use-package undo-tree :ensure t :defer t
  :init
  (add-hook 'prog-mode-hook #'undo-tree-mode)
  (add-hook 'markdown-mode-hook #'undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (let ((undo-dir (concat user-emacs-directory "undo")))
    (make-directory undo-dir t)
    (setq undo-tree-history-directory-alist `((".*" . ,undo-dir)))))

(use-package hl-todo :ensure t :defer t :if (display-graphic-p)
  :config
  (setq hl-todo-keyword-faces
        '(("HOLD" . "#d0bf8f")
          ("TODO" . "#cc9393")
          ("NEXT" . "#dca3a3")
          ("THEM" . "#dc8cc3")
          ("PROG" . "#7cb8bb")
          ("OKAY" . "#7cb8bb")
          ("DONT" . "#5f7f5f")
          ("FAIL" . "#8c5353")
          ("DONE" . "#afd8af")
          ("FIXME" . "#cc9393")
          ("XXX"   . "#cc9393")
          ("XXXX"  . "#cc9393")
          ("???"   . "#cc9393")))
;;; global-hl-todo-modeで有効にするメジャーモード(derived-mode)
  (setq hl-todo-activate-in-modes
        '(prog-mode
          markdown-mode)))

(use-package reveal-in-osx-finder :ensure t :defer t :if is-mac
  :bind (("C-x f" . reveal-in-osx-finder)))

(use-package flyspell-popup :ensure t :defer t :after flyspell
  :bind (:map
         flyspell-mode-map
         ("+" . flyspell-popup-correct)))

(use-package flyspell :ensure t :defer t
  :config
  (defun flyspell-toggle ()
    (interactive)
    (if flyspell-mode (flyspell-mode-off) (flyspell-mode)))
  (setq ispell-dictionary "english")
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))
  (advice-add 'flyspell-mode-on
              :before 'flyspell-buffer))

(provide 'config-editing)
