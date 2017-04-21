;; -----------------------------------
;; Programming utilities
;; -----------------------------------

;;; Code:

;;; Parenthesis settings
(use-package smartparens :ensure t
  :init (show-smartparens-global-mode t)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (sp-use-smartparens-bindings)
  (sp-local-pair '(emacs-lisp-mode
                   lisp-interaction-mode) "'" nil :actions nil)
  (sp-local-pair '(emacs-lisp-mode
                   lisp-interaction-mode) "`" nil :actions nil))

(setq show-paren-style 'parenthesis)
(show-paren-mode t)
(diminish 'show-paren-mode)
;; (define-advice show-paren-function (:around (fn) fix-show-paren-function)
;;   "Highlight enclosing parens."
;;   (cond ((looking-at-p "\\s(") (funcall fn))
;;         (t (save-excursion
;;              (ignore-errors (backward-up-list))
;;              (funcall fn)))))

(use-package rainbow-delimiters :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; Auto-completion settings
(use-package company :ensure t :defer t
  :bind (("C-c C-0" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-," . company-search-candidates)
         ("C-." . company-filter-candidates))
  :init
  (global-company-mode t)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        ;;company-begin-commands '(self-insert-command)
        )
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
  :init (company-flx-mode t))
(use-package company-statistics :ensure t :defer t :after company
  :config (add-hook 'after-init-hook 'company-statistics-mode))

(setq hippie-expand-try-function-list '(try-expand-debbrev
                                        try-expand-debbrev-all-buffers
                                        try-expand-debbrev-from-kill
                                        try-complete-file-name-partially
                                        try-complete-file-name
                                        try-expand-all-abbrevs
                                        try-expand-list
                                        try-expand-line
                                        try-complete-lisp-symbol-partially
                                        try-complete-lisp-symbol))

;;; Snippets settings
(use-package yasnippet :ensure t :defer t
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (yas-reload-all))

;;; Code linter settings
(use-package flycheck :ensure t :defer t
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
