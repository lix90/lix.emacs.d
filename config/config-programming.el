;; -----------------------------------
;; Programming utilities
;; -----------------------------------

;;; Code:
;; 开启文档模式
(add-hook 'prog-mode-hook #'eldoc-mode)

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

;; origami代码折叠
(use-package origami :ensure t :defer t)

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

;;; 自动补全
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
  (add-hook 'flycheck-mode-hook #'lix/use-eslint-from-node-modules)
  )

(use-package avy-flycheck :ensure t :defer t :after flycheck
  :config (avy-flycheck-setup))
(use-package flycheck-pos-tip :ensure t :defer t :after flycheck
  :init (setq-default tooltip-delay 0.2))

(provide 'config-programming)
;;; config-programming.el ends here
