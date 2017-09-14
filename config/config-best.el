;;; package --- Summary:

;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Code:

;;; 包管理
;;;=======
(use-package package-utils :ensure t :defer t)

(use-package el-get :ensure t :defer t :disabled t
  :init (setq el-get-verbose t))
(use-package paradox :ensure t :defer 30 :disabled t
  :commands (paradox-list-packages
             paradox-upgrade-packages)
  :config (setq paradox-execute-asynchronously t
                paradox-github-token t))
;; (setq paradox-github-token "d02fae45dd7c0c4845b56635d78448c63d7a7035")

;;; emacs server
;;;==============
;; (server-mode +1)

;;; 环境设定
;;;=========
(use-package exec-path-from-shell :ensure t :defer t :disabled t
  :if (memq window-system '(mac ns))
  :init
  ;; for speeding up startup time
  ;; see: https://github.com/purcell/exec-path-from-shell/issues/36
  (setq exec-path-from-shell-arguments '("-l"))
  ;; Solve warning of setting locale in ESS
  ;; from: https://stat.ethz.ch/pipermail/r-sig-mac/2015-October/011701.html
  ;; (exec-path-from-shell-copy-envs '("LC_ALL", "LANG"))
  (setq exec-path-from-shell-variables '("LC_ALL" "LANG"))
  (setq exec-path-from-shell-debug t)
  (exec-path-from-shell-initialize))

(unless (getenv "LANG") (setenv "LANG" "en_US.UTF-8"))
(unless (getenv "LC_ALL") (setenv "LC_ALL" "en_US.UTF-8"))
(setenv "MANPATH" (getenv "MANPATH"))
(when is-mac
  (setq exec-path '(
                    "/usr/local/bin"
                    "/usr/local/sbin"
                    "/bin"
                    "/sbin"
                    "/usr/bin"
                    "/usr/sbin"
                    "/Applications/Emacs.app/Contents/MacOS/bin"
                    "/Users/lix/.rvm/gems/ruby-2.3.0/bin"
                    "/Users/lix/.rvm/gems/ruby-2.3.0@global/bin"
                    "/Users/lix/.rvm/bin"
                    "/Users/lix/go/bin"
                    "/usr/local/mysql/bin"
                    )))
(when (not is-mac)
  (setq exec-path '(
                    "/usr/local/bin"
                    "/usr/local/sbin"
                    "/bin"
                    "/usr/bin"
                    "/opt/bin"
                    "~/.local/bin"
                    "/usr/lib/oracle/12.2/client64/bin"
                    )))
(setenv "PATH" (mapconcat 'identity exec-path ":"))

;;; 设置UTF-8编码方式
;;;===================
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)
;; disable CJK coding/encoding
(setq utf-translate-cjk-mode nil)

(setq-default
 create-lockfiles nil ;; 取消文件锁
 buffer-file-coding-system 'utf-8-unix ;; 默认编码系统
 make-backup-files nil ;; 取消文件备份
 tab-width 4 ;; tab为4个空格宽度
 case-fold-search t ;; 搜索和匹配时忽略大小写
 default-directory "~" ;; 当前buffer的默认路径
 fill-column 80 ;; 80栏宽, 当超过栏宽时，自动折叠行
 next-line-add-newlines nil ;; 下一行时插入新一行
 require-final-newline t ;; 自动添加新行至文件结尾
 mouse-yank-at-point t ;; 鼠标粘贴在光标处
 kill-whole-line t ;; kill整行
 indent-tabs-mode nil ;; 锁紧不适用tab符号
 x-select-enable-clipboard t ;; 开启剪切板
 select-enable-clipboard t
 x-select-enable-primary t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 visible-bell t ;; 提醒
 ediff-window-setup-function 'ediff-setup-windows-plain)
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;; MacOS系统的设置与功能增强
;;;=========================
(when is-mac
  (setq
   delete-by-moving-to-trash t ;; 删除时默认移至垃圾箱
   trash-directory "~/.Trash" ;; 垃圾箱路径
   ns-pop-up-frames nil
   ns-use-native-fullscreen t)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        mac-function-modifier 'hyper
        mac-right-option-modifier 'none))

;;; 切词模式
;;;=========
(use-package subword :defer t
  :diminish subword-mode
  :init
  (add-hook 'after-init-hook #'global-subword-mode))

;;; 删除选择
;;;========
(delete-selection-mode +1)
(diminish 'delete-selection-mode)

;;; 开启文档模式
;;;============
(use-package eldoc :defer t
  :diminish eldoc-mode
  :init (add-hook 'prog-mode-hook #'eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2
        eldoc-echo-area-use-multiline-p t))

;;; 保存历史
;;;=========
(use-package savehist :ensure t  :defer t
  :diminish savehist-mode
  :if (display-graphic-p)
  :init
  (add-hook 'after-init-hook #'savehist-mode)
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (concat user-emacs-directory ".cache/.savehist")))

;; 保存桌面
(use-package desktop :defer t :if (display-graphic-p)
  :init (add-hook 'after-init-hook #'desktop-save-mode)
  :config
  ;; Automatically save and restore sessions
  (make-directory (concat user-emacs-directory ".cache/desktop") t)
  (setq desktop-dirname (concat user-emacs-directory ".cache/desktop")
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
        desktop-locals-to-save nil))

;;; 保存最近打开的文件
(use-package recentf :defer t
  :diminish recentf-mode
  :init
  (add-hook 'after-init-hook #'recentf-mode)
  :config
  (setq recentf-save-file (concat user-emacs-directory ".cache/.recentf")
        recentf-max-saved-items 100
        recentf-max-menu-items 25))

;; 自动地保存文件位置，下次自动跳转到保存的位置
(use-package saveplace :ensure t :defer t
  :init
  (add-hook 'after-init-hook #'save-place-mode)
  :config
  (setq save-place-file
        (concat user-emacs-directory ".cache/.saveplace")))

;; 让两个同样文件名的文件具有唯一的buffer名
(use-package uniquify :defer t
  :init
  (setq uniquify-buffer-name-style 'forward))

;;; 视觉效果
;;;=========
(add-hook 'prog-mode-hook
          (lambda()
            (line-number-mode +1)
            (column-number-mode +1)))

;; 高亮行
(setq hl-line-sticky-flag nil)
(global-hl-line-mode +1)
(diminish 'hl-line-mode)

;; (setq visual-fill-column-width 85)
;; (global-visual-fill-column-mode +1)
;; (global-visual-line-mode +1)
;; (diminish 'visual-line-mode)
(add-hook 'minibuffer-setup-hook (lambda () (visual-line-mode -1)))
;; (add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(use-package linum :defer t
  :init
  (setq linum-format "%4s ")
  (setq linum-relative-format "%4s "))
(use-package hlinum :ensure t :defer t
  :init
  (add-hook 'prog-mode-hook #'hlinum-activate)
  :config
  (set-face-attribute 'linum-highlight-face nil
                      :background (face-attribute 'linum :background)
                      :foreground (face-attribute 'mode-line :background)
                      :weight 'bold))

;; 时间
(use-package time :defer t
  :init
  (add-hook 'after-init-hook #'display-time-mode)
  (setq display-time-format "%H:%M"))

(require 'ansi-color)
;; (setq-default ansi-color-for-comint-mode +1)

;; fringe
(fringe-mode '(0 . 8))

;;; 字体
(setq my/font-height (if is-mac 130 100)
      my/font-name "Source Code Pro")
(set-face-attribute 'default nil
                    :family my/font-name
                    :height my/font-height
                    :weight 'normal
                    :width 'normal)

(menu-bar-mode -1) ;; 不显示菜单栏
(when (fboundp 'tool-bar-mode) ;; 不显示工具栏
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) ;; 不显示滑动条
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) ;; 不显示水平滑动条
  (horizontal-scroll-bar-mode -1))
(when is-mac (setq mac-allow-anti-aliasing t)) ;; MacOS系统下开启防锯齿
(setq-default
 line-spacing 1 ;; 行距
 inhibit-startup-screen t ;; 关闭启动屏幕
 initial-scratch-message nil ;; 取消scrach文件信息
 cursor-in-non-selected-windows nil ;; 在非活动窗口不显示光标
 use-dialog-box nil ;; 使用对话框
 ring-bell-function 'ignore ;; 忽略提醒声
 message-log-max 10000 ;; 消息日志行数
 cursor-type 'bar ;; 光标类型
 )

(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b01110000
   #b00010000
   #b00010000
   #b00000000])

(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00001000
   #b00001000
   #b00001110
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;;; 主题doom-themes
;;;=================
(use-package doom-themes :ensure t :defer t
  :init (load-theme 'doom-vibrant t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-one-brighter-modeline t
        doom-themes-one-brighter-comments nil)
  (doom-themes-visual-bell-config)
  (add-hook 'org-mode-hook #'doom-themes-org-config))
;;; 切换主题
;;; https://github.com/habamax/.emacs.d/blob/master/lisp/haba-appearance.el
(defvar *my-theme-dark* 'doom-vibrant)
(defvar *my-theme-light* 'doom-one-light)
(defvar *my-current-theme* *my-theme-light*)

;; disable other themes before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

(defun my/next-theme (theme)
  (if (eq theme 'default)
      (disable-theme *my-current-theme*)
    (progn
      (load-theme theme t)))
  (setq *my-current-theme* theme))

(defun my/toggle-theme ()
  (interactive)
  (cond ((eq *my-current-theme* *my-theme-dark*)
         (my/next-theme *my-theme-light*))
        ((eq *my-current-theme* *my-theme-light*)
         (my/next-theme 'default))
        ((eq *my-current-theme* 'default)
         (my/next-theme *my-theme-dark*)))
  (set-face-attribute 'mode-line nil
                      :underline nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :underline nil
                      :box nil)
  (solaire-mode-swap-bg))
(bind-key* "C-<f9>" 'my/toggle-theme)

(use-package solaire-mode :ensure t :defer t
  :init (require 'solaire-mode)
  :config
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (solaire-mode-swap-bg))

;;; 导航
;;;======

;;; 使用god-mode方便导航
(use-package god-mode :ensure t :defer t
  :diminish god-mode
  :config
  (defun my/update-cursor ()
    (setq cursor-type
          (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'god-mode-enabled-hook 'my/update-cursor)
  (add-hook 'god-mode-disabled-hook 'my/update-cursor))
(global-set-key (kbd "<escape>") 'god-local-mode)

;;; 窗口导航
(use-package windmove :ensure t :defer t
  :init
  (windmove-default-keybindings)
  (defun delete-window-below ()
    "Delete window below. (require 'windmove)"
    (interactive)
    (windmove-down)
    (kill-this-buffer)
    (delete-window))
  (defun delete-window-above ()
    "Delete window above. (require 'windmove)"
    (interactive)
    (windmove-up)
    (kill-this-buffer)
    (delete-window))
  (defun delete-window-left ()
    (interactive)
    (windmove-left)
    (kill-this-buffer)
    (delete-windwo))
  (defun delete-window-right ()
    (interactive)
    (windmove-right)
    (kill-this-buffer)
    (delete-window)))

;; 使用goto-last-change前往上一次修改的位置
(use-package goto-last-change :ensure t :defer t
  :bind (("M-g b" . goto-last-change)))

;; 快速导航，移动光标至指定行，字符，词等
(use-package avy :ensure t :defer t
  :init
  (bind-keys ("M-g l" . avy-goto-line)
             ("M-g c" . avy-goto-char)
             ("M-g C" . avy-goto-char-2)
             ("M-g w" . avy-goto-word-0)
             ("M-g W" . avy-goto-word-1)))

;; dumb-jump快速跳转
(use-package dumb-jump :ensure t :defer t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("M-g q" . dumb-jump-quick-look))
  :config
  (setq dumb-jump-selector 'ivy
        dumb-jump-aggressive nil
        dumb-jump-prefer-searcher 'ag))

(use-package smooth-scrolling :ensure t :defer t
  :diminish smooth-scrolling-mode
  :config
  (setq smooth-scroll-margin 2)
  (setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil))

;;; 功能更加强劲的minibar
;;;=====================
(use-package which-key :ensure t :defer t
  :diminish which-key-mode
  :init
  (which-key-setup-minibuffer)
  (which-key-mode t)
  ;; (defalias 'display-buffer-in-major-side-window 'window--make-major-side-window)
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.30
        which-key-side-window-max-width 0.20
        which-key-max-description-length 20
        which-key-sort-order 'which-key-key-order
        which-key-idle-delay 0.1
        which-key-compute-remaps t))

;; 通过general可以拓展更多的快捷键
;; 但是暂时想简化使用，故暂时不使用
;; (use-package general :ensure t :defer t
;;   :init
;;   (general-create-definer
;;    leader-key
;;    :keymaps 'global
;;    :prefix (if is-mac "H-SPC"
;;              "s-SPC")))

(use-package undo-tree :ensure t :defer t
  :diminish undo-tree-mode
  :init
  (add-hook 'prog-mode-hook #'undo-tree-mode)
  (add-hook 'text-mode-hook #'undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (let ((undo-dir (concat user-emacs-directory ".cache/undo")))
    (make-directory undo-dir t)
    (setq undo-tree-history-directory-alist `((".*" . ,undo-dir)))))

;;; 文件编辑
;;;----------------------------------------------------------------------
;;;
;;; 设定写文件时的时间戳
(use-package time-stamp :ensure t :defer t
  :config
  (setq time-stamp-active t          ; do enable time-stamps
        time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
        time-stamp-format "Last modified on %04y-%02m-%02d %02H:%02M:%02S (%U)")
  (add-hook 'write-file-hooks #'time-stamp))

;;; 文件自动保存
;;; 当idle或者光标失焦（lose focus）时，自动保存文件
(use-package super-save :ensure t :defer t
  :diminish super-save-mode
  :init
  (add-hook 'after-init-hook #'super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t
        auto-save-default nil))

;;; 标记mark
;;;==========
;; 智能标记
(use-package expand-region :ensure t :defer t
  :bind ("C-=" . er/expand-region)
  :init
  (bind-keys :prefix-map expand-region-map
             :prefix "M-s e"
             :prefix-docstring "Expand Region"
             ("f" . er/mark-defun)
             ("c" . er/mark-comment)
             ("e" . er/mark-email)
             ("s" . er/mark-sentence)
             ("p" . er/mark-paragraph)))
(use-package phi-rectangle :ensure t :defer t
  :bind (("C-x s" . phi-rectangle-set-mark-command)))
(bind-key "C-S-M" 'set-mark-command)
;;; ----------------------------------------------------------------------
;;; 搜索与替换工具
;;;
;;
;; anzu指南
;; 1. query-replace 查询、代替
;; 2. enter
;; 3. y -> act,
;;    n -> skip,
;;    Y -> act all,
;;    N -> skip all,
;;    , -> act but not next
(use-package anzu :ensure t :defer t
  :diminish anzu-mode
  :init (add-hook 'prog-mode-hook #'anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;; iedit指南：
;; 用于批量修改匹配的symbol或者word或者region
;; C-;/M-s ; 开启或关闭iedit-mode
;; 在iedit-mode开启时，通过C-' 可以仅仅显示选中词出现的段落，隐藏其他段落
;; 在iedit模式时，编辑一个选区将同时改变其他选取，也就是说，可以当做多光标模式使用
(use-package iedit :ensure t :defer t
  :init
  (bind-key* "C-;" 'iedit-mode))

;; 需安装the_silver_searcher
;; gentoo linux: `emerge -avt the_silver_searcher`
;; macos: `brew install the_silver_searcher`
;; 可以与projectile进行绑定，在项目中搜索
(use-package ag :ensure t :defer t
  :init
  (bind-keys :prefix-map ag-search
             :prefix "M-s f"
             :prefix-docstring "Ag Search"
             ("p" . ag-project)
             ("f" . ag-files)
             ("F" . ag-project-files)))

;;;----------------------------------------------------------------------=
;; swiper指南
;; 1. C-s带开搜索窗口
;; 2. 输入搜索字段
;; 3. C-' 使用avy定位行
;; 4. M-q 替换搜索字段（query-replace）

;;; ivy指南
;;;
;;; 在ivy模式时
;;; - 确认可以按Enter或者C-m
;;; - 完成或者补全按TAB或者C-j
;;; - 立即完成（不使用自动匹配）按C-M-j
;;; - 调度动作按M-o，动作根据不同模式变化
;;; - 调用avy进行搜索跳转光标，按C-'
;;;
;;; counsel指南
;;;
(use-package swiper :ensure t :defer t
  :diminish ivy-mode
  :bind (("M-x" . counsel-M-x)
         ("C-s" . counsel-grep-or-swiper)
         ("M-s f" . counsel-find-file)
         ("M-s j" . counsel-file-jump)
         ("M-s p" . counsel-ag)
         ("M-s r" . counsel-recentf)
         ("M-s b" . counsel-bookmark)
         ("M-s y" . counsel-yank-pop)
         ("M-s C-a" . counsel-linux-app)
         ("M-s C-i" . counsel-imenu)
         ("M-s C-l" . counsel-find-library)
         ("M-s C-f" . counsel-faces)
         ("M-s C-u" . counsel-unicode-char)
         )
  :init
  (add-hook 'after-init-hook #'ivy-mode)
  :config
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-fixed-height-minibuffer t
        ivy-height 20
        ivy-use-virtual-buffers t
        ivy-display-style 'fancy)
  ;;advise swiper to recenter on exit
  (defun my/swiper-recenter (&rest args)
    "recenter display after swiper"
    (recenter))
  (advice-add 'swiper :after #'my/swiper-recenter))

;; 提供模糊搜索
(use-package flx :ensure t :defer t)

;;;----------------------------------------------------------------------
;;; imenu使用指南
;;;
;;; imenu
;;; 能够分析当前缓冲区中的定义，并生成索引。
;;; 对于结构化文档，它生成标题和章节的大纲。
(use-package imenu-anywhere :ensure t :after ivy
  :bind ("M-s i" . ivy-imenu-anywhere))

(use-package counsel-projectile :ensure t :defer t
  :bind (("C-x C-p" . counsel-projectile-find-file)))

;;; ----------------------------------------------------------------------
;;; 括号的操作与编辑
;;;
;;; smartparens智能括号操作，较难掌握
(use-package smartparens :ensure t :defer t
  :diminish smartparens-mode
  :init
  (add-hook 'prog-mode-hook #'show-smartparens-mode)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (add-hook 'markdown-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-local-pair '(emacs-lisp-mode
                   lisp-interaction-mode) "'" nil :actions nil)
  (sp-local-pair '(emacs-lisp-mode
                   lisp-interaction-mode) "`" nil :actions nil)
  (sp-local-pair '(emacs-lisp-mode
                   lisp-interaction-mode) "{" nil :actions nil))

(setq show-paren-style 'parenthesis)
(show-paren-mode t)
(diminish 'show-paren-mode)
;; (define-advice show-paren-function (:around (fn) fix-show-paren-function)
;;   "Highlight enclosing parens."
;;   (cond ((looking-at-p "\\s(") (funcall fn))
;;         (t (save-excursion
;;              (ignore-errors (backward-up-list))
;;              (funcall fn)))))

;; 多彩括号
(use-package rainbow-delimiters :ensure t :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; 自动补全与代码片段
;;;=================
;; 设置hippie-expand尝试函数
(use-package hippie-exp :ensure t :defer t
  :bind ("M-/" . hippie-expand)
  :config
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
          try-complete-lisp-symbol)))

;;;-----------------------------------------------------------------------------
;; abbrev mode
;;;-----------------------------------------------------------------------------
;; C-x a abbrev key map
;; C-x ' expand abbrev
;; You can use abbrev for:
;; * Long English words, e.g. “comm” for “communication”.
;; * Programing language function templates.
;; * emoji, such as “omg” for
;; * math symbols such as “ra” for →
;; * Templates, such as license header.
;; * Address, url, telephone number, company name, etc.
(use-package abbrev :defer t
  :config
  (setq save-abbrevs 'silently)
  (setq abbrev-file-name
        (concat user-emacs-directory ".cache/.abbrev" ))
  (dolist (hook '(prog-mode-hook
                  text-mode-hook))
    (add-hook 'hook #'abbrev-mode)))

(use-package dabbrev :defer t)

(use-package company :ensure t :defer t
  :diminish company-mode
  :bind (("C-c C-0" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-," . company-search-candidates)
         ("C-." . company-filter-candidates))
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  :config
  (unbind-key "RET" company-active-map)
  (unbind-key "<return>" company-active-map)
  (bind-key "<tab>" #'company-complete-selection company-active-map)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-begin-commands '(self-insert-command))
  ;; set default `company-backends'
  (setq company-backends
        '(company-capf
          (company-abbrev company-dabbrev company-keywords)
          company-files
          company-yasnippet
          ))
  ;; Nicer looking faces
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

(use-package company-flx :ensure t :defer t :after company
  :init
  (add-hook 'company-mode-hook #'company-flx-mode))

(use-package company-statistics :ensure t :defer t :after company
  :init
  (setq company-statistics-file
        (concat user-emacs-directory ".cache/.company-statistics" )))

;;; alternatives
(use-package auto-complete :ensure t :defer t)

;;; Snippets settings
(use-package yasnippet :ensure t :defer t
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode))

;;; 代码调试
;;;=========
;;; Code linter settings
(use-package flycheck :ensure t :defer t :if (display-graphic-p)
  :diminish flycheck-mode
  :bind (:map flycheck-mode-map
              ("C-c f l" . flycheck-list-errors)
              ("C-c f n" . flycheck-next-error)
              ("C-c f p" . flycheck-previous-error))
  :config
  (setq flycheck-javascript-standard-executable "standard"
        flycheck-javascript-eslint-executable "eslint"
        flycheck-eslintrc ".eslintrc.json"
        flycheck-temp-prefix ".flycheck"
        flycheck-highlighting-mode 'lines
        flycheck-indication-mode nil)
  (setq-default flycheck-disabled-checkers '(javascript-jshint
                                             json-jsonlist))
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'js3-mode)
  (flycheck-add-mode 'javascript-standard 'rjsx-mode)
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun lix/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'lix/use-eslint-from-node-modules))

(use-package avy-flycheck :ensure t :defer t :after flycheck
  :config (avy-flycheck-setup))
(use-package flycheck-pos-tip :ensure t :defer t :after flycheck
  :init (setq-default tooltip-delay 0.2))

;;;----------------------------------------------------------------------
;;; 文件管理
;;;----------------------------------------------------------------------
;;; doc-view mode
(use-package doc-view :defer t
  :config
  (bind-keys :map doc-view-mode-map
             ("C-o" . counsel-osx-app)))

;;; ido
(use-package ido :defer t
  :config
  (setq ido-save-directory-list-file
        (concat user-emacs-directory ".cache/.ido_last" )))

(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; bookmark & bookmark+
;;; 列出当前的书签列表 C-x r l
(use-package bookmark
  :config
  (setq bookmark-default-file
        (concat user-emacs-directory ".cache/.emacs-bookmarks.el" )))
(use-package bookmark+ :defer t
  :config
  (setq bmkp-bmenu-state-file
        (concat user-emacs-directory ".cache/.emacs-bmk-bmenu-state.el")
        bmkp-bmenu-commands-file
        (concat user-emacs-directory ".cache/.emacs.bmk-bmenu-commands.el")))

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  ;; allow dired to delete or copy dir
  ;; “always” means no asking
  (setq dired-recursive-copies 'always)
  ;; “top” means ask once
  (setq dired-recursive-deletes 'top)
  (setq dired-listing-switches "-alh")
  (setq insert-directory-program
        (or (executable-find "gls")
            (executable-find "ls")))
  (setq dired-dwim-target t)
  (define-key dired-mode-map (kbd "RET")
    'dired-find-alternate-file)
  ;; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ;;; 其他dired拓展功能的包
  (use-package dired+ :ensure t :defer t)
  (use-package dired-single :ensure t :defer t)
  (use-package dired-details :ensure t :defer t)
  (use-package dired-details+ :ensure t :defer t)
  (use-package dired-k :ensure t :defer t)
  (require 'dired+)
  (add-hook 'dired-initial-position-hook #'dired-k))

(use-package neotree :ensure t
  :bind (("<f7>" . neotree-toggle)
         :map neotree-mode-map
         ("RET" . neotree-enter)
         ("TAB" . neotree-stretch-toggle)
         ("|" . neotree-enter-vertical-split)
         ("-" . neotree-enter-horizontal-split)
         ("'" . neotree-quick-look)
         ("c" . neotree-create-node)
         ("C" . neotree-copy-node)
         ("d" . neotree-dir)
         ("D" . neotree-delete-node)
         ("g" . neotree-refresh)
         ("h" . spacemacs/neotree-collapse-or-up)
         ("H" . neotree-select-previous-sibling-node)
         ("n" . neotree-next-line)
         ("N" . neotree-down-node)
         ("p" . neotree-previous-line)
         ("P" . neotree-select-up-node)
         ("l" . spacemacs/neotree-expand-or-open)
         ("L" . neotree-select-next-sibling-node)
         ("q" . neotree-hide)
         ("R" . neotree-rename-node)
         ("r" . neotree-change-root)
         ("s" . neotree-hiden-file-toggle)
         )
  :config
  (setq-default neo-keymap-style 'concise)
  ;;(setq-default neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-theme 'icon)
  (setq  neo-show-hidden-files t
         neo-modern-sidebar t
         neo-show-updir-line nil
         neo-dont-be-alone t
         neo-banner-message nil
         neo-create-file-auto-open t
         neo-window-width 24
         neo-window-fixed-size t
         neo-smart-open t
         neo-auto-indent-point t
         neo-confirm-delete-directory-recursively t
         neo-confirm-delete-file t
         neo-vc-integration nil
         neo-mode-line-type 'none)
  ;; Solved this problem
  ;; https://github.com/jaypei/emacs-neotree/issues/50
  (setq-default neo-persist-show t)
  (when neo-persist-show
    (add-hook 'popwin:before-popup-hook
              (lambda () (setq neo-persist-show nil)))
    (add-hook 'popwin:after-popup-hook
              (lambda () (setq neo-persist-show t))))
             ;;; Code retrieved from https://github.com/jaypei/emacs-neotree/issues/218

  (defun lix/text-scale-twice ()
    "Text scale for neotree."
    (interactive)
    (progn
      (text-scale-adjust 0)
      (text-scale-decrease 1)))
  (add-hook 'neo-after-create-hook
            (lambda (_)
              ;;(call-interactively 'lix/text-scale-twice)
              (visual-line-mode -1)
              (setq truncate-lines t)))

  (defun spacemacs/neotree-expand-or-open ()
    "Expand or open a neotree node."
    (interactive)
    (let ((node (neo-buffer--get-filename-current-line)))
      (when node
        (if (file-directory-p node)
            (progn
              (neo-buffer--set-expand node t)
              (neo-buffer--refresh t)
              (when neo-auto-indent-point
                (next-line)
                (neo-point-auto-indent)))
          (call-interactively 'neotree-enter)))))

  (defun spacemacs/neotree-collapse ()
    "Collapse a neotree node."
    (interactive)
    (let ((node (neo-buffer--get-filename-current-line)))
      (when node
        (when (file-directory-p node)
          (neo-buffer--set-expand node nil)
          (neo-buffer--refresh t))
        (when neo-auto-indent-point
          (neo-point-auto-indent)))))

  (defun spacemacs/neotree-collapse-or-up ()
    "Collapse an expanded directory node or go to the parent node."
    (interactive)
    (let ((node (neo-buffer--get-filename-current-line)))
      (when node
        (if (file-directory-p node)
            (if (neo-buffer--expanded-node-p node)
                (spacemacs/neotree-collapse)
              (neotree-select-up-node))
          (neotree-select-up-node)))))

  (defun neotree-find-project-root ()
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (let ((origin-buffer-file-name (buffer-file-name)))
        (neotree-find (projectile-project-root))
        (neotree-find origin-buffer-file-name))))

  (defun spacemacs//neotree-maybe-attach-window ()
    (when (get-buffer-window (neo-global--get-buffer))
      (neo-global--attach))))

(use-package projectile :ensure t :defer t
  :diminish projectile-mode
  :init (add-hook 'after-init-hook #'projectile-mode)
  :config
  (setq projectile-cache-file
        (concat user-emacs-directory ".cache/.projectile" )
        projectile-known-projects-file
        (concat user-emacs-directory ".cache/.projectile-bookmarks" )))


;;; ----------------------------------------------------------------------
;;; 其他小工具
;;; ----------------------------------------------------------------------
;; 基于`expand-region`增加/变更/删除pairs
;; - add
;; - change
;; - delete
(use-package embrace :ensure t
  :bind ("C-," . embrace-commander))

;; 饥饿删除模式
(use-package hungry-delete :ensure t :defer t
  :diminish hungry-delete-mode
  :init (global-hungry-delete-mode +1))

(use-package whole-line-or-region :ensure t :defer t :disabled t
  :diminish whole-line-or-region-mode
  :bind (("M-w" . whole-line-or-region-copy-region-as-kill)
         ("C-y" . whole-line-or-region-yank)
         ("C-." . whole-line-or-region-comment-dwim))
  :init (whole-line-or-region-global-mode +1))

(use-package crux :ensure t :defer t
  :bind (("C-a" . crux-move-beginning-of-line))
  :init
  (bind-keys :prefix-map Manipulate-line
             :prefix "C-o"
             :prefix-docstring "Open line"
             ("o" . open-line)
             ("j" . join-line)
             ("s" . split-line)
             ("d" . crux-smart-open-line)
             ("a" . crux-smart-open-line-above)
             ("k" . crux-kill-whole-line)))

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

;; 自动清除空白
(use-package whitespace-cleanup-mode :ensure t :defer t
  :diminish whitespace-cleanup-mode
  :init (add-hook 'prog-mode-hook #'whitespace-cleanup-mode))

;; 暴力缩进
(use-package aggressive-indent :ensure t :defer t
  :diminish aggressive-indent-mode
  :init (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-excluded-modes
               '(python-mode
                 haml-mode
                 html-mode)))

;;; ----------------------------------------------------------------------
;;; 设定快捷键
;;;
;;; 常用快捷键记录
;;; - kill括号
;;;
(bind-key* "C-S-d" 'kill-word)
(bind-key* "M-s s" 'isearch-forward-regexp)
(bind-key* "M-s S" 'isearch-backward-regexp)
(bind-key* "M-s l" 'counsel-find-library)
(bind-key* "M-s I" 'imenu)
(bind-key "C-S-B" 'backward-sexp)
(bind-key "C-S-F" 'forward-sexp)

;;; 按组来设定快捷键
(use-package hydra :ensure t :defer t
  :init
  (defhydra hydra-zoom (global-map "C-c")
    "zoom"
    ("+" text-scale-increase "larger")
    ("-" text-scale-decrease "smaller")
    ("0" text-scale-adjust "recover"))
  (defhydra hydra-adjwin (global-map "C-c")
    "adjust window"
    ("<left>" shrink-window-horizontally "shrink horizontally")
    ("<right>" enlarge-window-horizontally "enlarge horizontally")
    ("<up>" enlarge-window "enlarge vertically")
    ("<down>" shrink-window "shrink vertically")
    ("B" balance-windows "balance")))

;;; 模式开关
(bind-keys :prefix "C-x 7"
           :prefix-map my/toggle-toggle
           ("l" . linum-mode)
           ("L" . hlinum-mode)
           ("h" . hl-line-mode)
           ("v" . visual-line-mode)
           ("c" . visual-fill-column-mode)
           ("t" . my/toggle-theme)
           ("0" . my/toggle-transparency))

(bind-keys :prefix "M-s t"
           :prefix-map helpful-tools
           ("d" . insert-date-and-time)
           ("D" . insert-date))

(provide 'config-best)
;;; config-best.el ends here
