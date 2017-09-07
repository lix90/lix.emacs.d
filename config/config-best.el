;;;-----------------------------------------------------------------------------
;;; 包管理
;;;-----------------------------------------------------------------------------
(use-package el-get :ensure t :defer t :disabled t
  :init (setq el-get-verbose t))
(use-package paradox :ensure t :defer 30
  :commands (paradox-list-packages
             paradox-upgrade-packages)
  :config (setq paradox-execute-asynchronously t
                paradox-github-token t))
(setq paradox-github-token "d02fae45dd7c0c4845b56635d78448c63d7a7035")
(use-package package-utils :ensure t :defer 30)

;;;-----------------------------------------------------------------------------
;;; 环境设定
;;;-----------------------------------------------------------------------------
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
                    )))

(when (not is-mac)
  (setq exec-path '(
                    "/usr/local/bin"
                    "/usr/local/sbin"
                    "/bin"
                    "/usr/bin"
                    "/opt/bin"
                    )))

(setenv "PATH" (mapconcat 'identity exec-path ":"))

;;; ----------------------------------------------------------------------
;;; 将常用的命令绑定快捷键，不断增加



(global-set-key (kbd "M-s s") 'isearch-forward-regexp)
(global-set-key (kbd "M-s r") 'isearch-backward-regexp)

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

;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 设置emacs内置参数
(setq-default
 ;; 取消文件锁
 create-lockfiles nil
 ;; 默认编码系统
 buffer-file-coding-system 'utf-8-unix
 ;; 取消文件备份
 make-backup-files nil
 ;; tab为4个空格宽度
 tab-width 4
 case-fold-search t
 default-directory "~"
 fill-column 80
 next-line-add-newlines nil
 require-final-newline t
 ;;mouse-yank-at-point t ;; alrealy in better-default
 kill-whole-line t
 indent-tabs-mode nil
 x-select-enable-clipboard t
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

;;; ----------------------------------------------------------------------
;;; macos系统的设置与功能增强
;;;

(when is-mac
  (setq
   ;; 删除时默认移至垃圾箱
   delete-by-moving-to-trash t
   ;; 垃圾箱路径
   trash-directory "~/.Trash"
   ns-pop-up-frames nil
   ns-use-native-fullscreen nil)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        mac-function-modifier 'hyper
        mac-right-option-modifier 'none))

;; 在osx-finder中打开
(use-package reveal-in-osx-finder :ensure t :defer t :if is-mac)

;;; ----------------------------------------------------------------------
;;; emacs内置mode设置
;;
;; 切词模式
(global-subword-mode +1)
;; 删除选择
(delete-selection-mode +1)
;; 内置时间mode设定
;; (setq display-time-format "%a %b %d | %H:%M |")
;; (display-time-mode +1)
;; 开启文档模式
(add-hook 'prog-mode-hook #'eldoc-mode)

;;;-----------------------------------------------------------------------------
;; hippie-expand
;;;-----------------------------------------------------------------------------
;; 设置hippie-expand尝试函数
(global-set-key (kbd "M-/") 'hippie-expand)
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
(dolist (hook (list
               prog-mode-hook
               text-mode-hook
               ))
  (add-hook 'hook #'abbrev-mode))
(setq save-abbrevs 'silently)
(setq abbrev-file-name
      (concat user-emacs-directory "emacs_abbre.el"))
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;; 保存历史
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
        desktop-locals-to-save nil))

;; 保存最近打开的文件
(use-package recentf :ensure t :defer t
  :init (add-hook 'after-init-hook #'recentf-mode)
  :config
  (setq recentf-save-file (concat user-emacs-directory "recentf")
        recentf-max-saved-items 100
        recentf-max-menu-items 25))

;; saveplace
;; 自动地保存文件位置，下次自动跳转到保存的位置
(require 'saveplace)
(setq-default save-place t)
;; uniquify
;; 让两个同样文件名的文件具有唯一的buffer名
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

;;; ----------------------------------------------------------------------
;;; 视觉效果
;;; ----------------------------------------------------------------------
;;;
(require 'ansi-color)
(setq-default ansi-color-for-comint-mode +1)
;; 颜色设定
(defconst hl-color "#ffd446")
(defconst hl-color-dark "#282c34")
;; fringe
(fringe-mode '(0 . 2))
;; 字体高度
(setq my/font-height (if is-mac 130 100)
      my/font-name "Source Code Pro")
;; 设置字体
(set-face-attribute 'default nil
                    :family my/font-name
                    :height my/font-height
                    :weight 'normal
                    :width 'normal)

;; 不显示菜单栏
(menu-bar-mode -1)
;; 不显示工具栏
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; 不显示滑动条
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; 不显示水平滑动条
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
;; MacOS系统下开启防锯齿
(when is-mac
  (setq mac-allow-anti-aliasing t))
(setq-default
 ;; 行距
 line-spacing 1
 ;; 关闭启动屏幕
 inhibit-startup-screen t
 ;; 取消scrach文件信息
 initial-scratch-message nil
 ;; 在非活动窗口不显示光标
 cursor-in-non-selected-windows nil
 ;; 使用对话框
 use-dialog-box nil
 ;; 忽略提醒声
 ring-bell-function 'ignore
 ;; 消息日志行数
 message-log-max 10000
 ;; 行号设定
 linum-format "%4s "
 ;; 光标类型
 cursor-type 'bar
 )
;; 相对行号
;;(setq linum-relative-format "%4s ")
;; 设定光标背景色
(set-face-background 'cursor hl-color)

(dolist (hook '(prog-mode-hook
                web-mode-hook
                matlab-mode-hook))
  (add-hook 'hook
            (lambda()
              (visual-line-mode +1)
              (global-visual-line-mode -1)
              ;; line number
              (linum-mode +1)
              ;; highlight current line
              (hl-line-mode +1)
              (set-face-background 'hl-line hl-color-dark)
              (toggle-truncate-lines +1)
              )))

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


;; 高亮行号
(use-package hlinum :ensure t :defer t
  :init
  (add-hook 'prog-mode-hook #'hlinum-activate)
  :config
  (set-face-attribute 'linum-highlight-face nil
                      :background hl-color-dark
                      :foreground hl-color
                      :weight 'bold))

;;; ----------------------------------------------------------------------
;;; 主题
;;; ----------------------------------------------------------------------
;;; GUI主题：doom-themes
(use-package doom-themes :ensure t :defer t
  :if (display-graphic-p)
  :config
  (setq doom-enable-bold t
        doom-enable-italic t
        doom-one-brighter-modeline t
        doom-one-brighter-comments nil))
(if (display-graphic-p)
    (load-theme 'doom-one t))

;;; ----------------------------------------------------------------------
;;; 第三方包设定
;;;
;;; 1. 导航
;;; - God-mode
;;; ----------------------------------------------------------------------
;;; 使用god-mode方便导航
(use-package god-mode :ensure t :defer t
  :config
  (defun lix/update-cursor ()
    (setq cursor-type
          (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'god-mode-enabled-hook 'lix/update-cursor)
  (add-hook 'god-mode-disabled-hook 'lix/update-cursor))
(global-set-key (kbd "<escape>") 'god-local-mode)

;; navigate windows & delete windows
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
(use-package goto-last-change :ensure t
  :bind (("M-g l" . goto-last-change)))

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

;; dumb-jump快速跳转
;; 使用ivy选择
;; 使用ag搜索
(use-package dumb-jump :ensure t :defer t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("M-g q" . dumb-jump-quick-look))
  :init
  (setq dumb-jump-selector 'ivy
        dumb-jump-aggressive nil
        dumb-jump-prefer-searcher 'ag))

(use-package smooth-scrolling :ensure t :defer t
  :init (add-hook 'prog-mode-hook #'smooth-scrolling-mode)
  :config
  (setq smooth-scroll-margin 2)
  (setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil))

;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; 工具增强
;;;
;;; 功能更加强劲的minibar
(use-package which-key :ensure t
  :init
  (which-key-setup-minibuffer)
  (which-key-mode t)
  ;; (defalias 'display-buffer-in-major-side-window 'window--make-major-side-window)
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.30
        which-key-side-window-max-width 0.20
        which-key-max-description-length 25
        which-key-allow-evil-operators t
        which-key-sort-order 'which-key-key-order
        which-key-unicode-correction 3
        which-key-prefix-prefix "+"
        which-key-idle-delay 0.15))

;; 通过general可以拓展更多的快捷键
;; 但是暂时想简化使用，故暂时不使用
;; (use-package general :ensure t :defer t
;;   :init
;;   (general-create-definer
;;    leader-key
;;    :keymaps 'global
;;    :prefix (if is-mac "H-SPC"
;;              "s-SPC")))

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

;;; ----------------------------------------------------------------------
;;; 文件编辑
;;
;; 设定写文件时的时间戳
(use-package time-stamp :ensure t :defer t
  :config
  (setq time-stamp-active t          ; do enable time-stamps
        time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
        time-stamp-format "Last modified on %04y-%02m-%02d %02H:%02M:%02S (%U)")
  (add-hook 'write-file-hooks #'time-stamp))

;; 文件自动保存
;; 当idle或者光标失焦（lose focus）时，自动保存文件
(use-package super-save :ensure t :defer t
  :init
  (add-hook 'after-init-hook #'super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t
        auto-save-default nil))

;;;-----------------------------------------------------------------------------
;;; 标记mark
(global-set-key (kbd "C-:") 'set-mark-command)

;;; ----------------------------------------------------------------------
;;; 搜索与替换工具
;;;
;;
;; anzu tutorial
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

;; learn iedit
;; 用于批量修改匹配的symbol或者word或者region
;; 基本用法：
;; C-;/M-s ; 开启或关闭iedit-mode
;; 在iedit-mode开启时，通过C-' 可以仅仅显示选中词出现的段落，
;; 隐藏其他段落
(use-package iedit :ensure t :defer t
  :init
  (bind-key* "C-;" 'iedit-mode))

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

(use-package imenu-anywhere :ensure t :after ivy
  :bind ("M-s i" . ivy-imenu-anywhere))

(use-package flx :ensure t :defer t)

(use-package swiper :ensure t :defer t :ensure counsel
  :commands (ivy-resume counsel-M-x counsel-find-file)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . counsel-grep-or-swiper)
         ("C-c i" . ivy-immediate-done)
         ("M-s p" . counsel-ag))
  :init (add-hook 'after-init-hook #'ivy-mode)
  :config
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-fixed-height-minibuffer t
        ivy-height 20
        ivy-use-virtual-buffers t
        ivy-display-style 'fancy)
  ;;advise swiper to recenter on exit
  (defun lix/swiper-recenter (&rest args)
    "recenter display after swiper"
    (recenter))
  (advice-add 'swiper :after #'lix/swiper-recenter))

;;; ----------------------------------------------------------------------
;;; 括号的操作与编辑
;;;
;;; smartparens智能括号操作，较难掌握
(use-package smartparens :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook #'show-smartparens-mode)
    (add-hook 'prog-mode-hook #'smartparens-mode)
    (add-hook 'markdown-mode-hook #'smartparens-mode))
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
(diminish 'show-paren-mode "")
;; (define-advice show-paren-function (:around (fn) fix-show-paren-function)
;;   "Highlight enclosing parens."
;;   (cond ((looking-at-p "\\s(") (funcall fn))
;;         (t (save-excursion
;;              (ignore-errors (backward-up-list))
;;              (funcall fn)))))

;; 多彩括号
(use-package rainbow-delimiters :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; ----------------------------------------------------------------------
;;; 自动补全与代码片段
;;; ----------------------------------------------------------------------
(use-package company :ensure t :defer t
  :bind (("C-c C-0" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-," . company-search-candidates)
         ("C-." . company-filter-candidates))
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  :config
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-begin-commands '(self-insert-command))
  ;; set default `company-backends'
  (setq-default company-backends
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
  (company-flx-mode t))
(use-package company-statistics :ensure t :defer t :after company)

;;; Snippets settings
(use-package yasnippet :ensure t :defer t
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (yas-reload-all))

;;; ----------------------------------------------------------------------
;;; 代码调试
;;; ----------------------------------------------------------------------

;;; Code linter settings
(use-package flycheck :ensure t :defer t :if (display-graphic-p)
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

;;; ----------------------------------------------------------------------
;;; 文件管理
;;; ----------------------------------------------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)
(use-package dired+ :ensure t :defer t)
(use-package dired-single :ensure t :defer t)
(use-package dired-details+ :defer t :ensure dired-details)
(bind-key* "C-S-d" 'kill-word)
(bind-keys :prefix-map lix/dired-map
           :prefix "M-d"
           :prefix-docstring "Dired Map"
           ("j" . dired-jump)
           ("d" . ido-dired))
(add-hook 'dired-mode-hook
          (lambda ()
            (put 'dired-find-alternate-file 'disabled nil)
            (dired-hide-details-mode t)
            (hl-line-mode t)
            ;; allow dired to delete or copy dir
            (setq dired-details-hidden-string " ")
            (setq dired-recursive-copies 'always) ; “always” means no asking
            (setq dired-recursive-deletes 'top) ; “top” means ask once
            (setq insert-directory-program (or (executable-find "gls")
                                               (executable-find "ls")))
            (setq dired-dwim-target t)
            (define-key dired-mode-map (kbd "RET")
              'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))

(use-package neotree :ensure t
  :commands (neotree-toggle)
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
  :config
  (projectile-global-mode +1)
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package counsel-projectile :ensure t :defer t
  :init
  (counsel-projectile-on))

(use-package find-file-in-project :ensure t :defer t)

;;; ----------------------------------------------------------------------
;;; 其他小工具
;;; ----------------------------------------------------------------------
;; 基于`expand-region`增加/变更/删除pairs
;; add
;; change
;; delete
(use-package embrace :ensure t
  :bind ("C-," . embrace-commander))

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

;; 自动清除空白
(use-package whitespace-cleanup-mode :ensure t :defer t
  :init (add-hook 'prog-mode-hook #'whitespace-cleanup-mode))

;; 暴力缩进
(use-package aggressive-indent :ensure t :defer t
  :init (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-excluded-modes
               '(python-mode
                 haml-mode
                 html-mode)))



(provide 'config-best)
