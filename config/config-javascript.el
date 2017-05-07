;;;----------------------------------------
;; JavaScript mode
;;;----------------------------------------
;; syntax checking
;; strict mode warnings
;; undeclared variable warnings
;; smart line wrapping
;; code folding
;; code browsing
;;

(add-hook 'js-mode-hook
          (lambda()
            (setq js-indent-level 2)
            (js2-minor-mode +1)))

(use-package js2-mode :ensure t :defer t
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :mode (("\\.\\(js\\|json\\)$" . js2-mode)
         ("\\.jsx?$" . js2-jsx-mode))
  :config
  (progn
    (setq js2-indent-level 2
          js2-basic-offset 2
          js2-highlight-level 3
          js2-mode-show-parse-errors t
          js2-mode-show-strict-warnings t)
    (add-hook 'js2-mode-hook
              (lambda ()
                (local-set-key (kbd "RET") 'newline-and-indent)
                ;; minor mode
                (yas-minor-mode t)
                (tern-mode t)
                (hl-line-mode t)
                (smartparens-mode t)
                ;;(indium-interaction-mode t)
                ))
    (add-hook 'js2-mode-hook
              (lambda ()
                ;; set local company-mode
                (add-to-list 'company-backends
                             '(company-tern))
                (company-mode +1)))))

(use-package indium :ensure t :defer t :disabled t
  :config
  (add-hook 'js2-mode-hook #'indium-interaction-mode))

(use-package js3-mode :ensure t :disabled t
  :interpreter (("node" . js3-mode))
  :mode ("\\.\\(js\\|json\\)$" . js3-mode)
  :config
  (setq js3-auto-indent-p t
        js3-curly-indent-offset 0
        js3-enter-indents-newline t
        js3-expr-indent-offset 2
        js3-indent-on-enter-key t
        js3-lazy-commas t
        js3-lazy-dots t
        js3-lazy-operators t
        js3-paren-indent-offset 2
        js3-square-indent-offset 4)
  (add-hook 'js3-mode-hook
            (lambda()
              (hl-line-mode t)
              (tern-mode t)
              (smartparens-mode t)
              (yas-minor-mode)
              ;;(flycheck-mode t)
              ;; jade interaction
              (jade-interaction-mode t)))
  (add-hook 'js3-mode-hook
            (lambda ()
              ;; set local company-mode
              (add-to-list 'company-backends
                           '(company-tern)))))

(use-package tern :ensure t :defer t
  :config
  (setq tern-command (cons (executable-find "tern") '())))

(use-package nodejs-repl :ensure t :defer t
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (define-key js2-mode-map (kbd "C-c j /") 'nodejs-repl-get-candidates)
              (define-key js2-mode-map (kbd "C-c j C-l") 'nodejs-repl-load-file)
              (define-key js2-mode-map (kbd "C-c j C-z") 'nodejs-repl-switch-to-repl)
              (define-key js2-mode-map (kbd "C-c j s r") 'nodejs-repl-send-region)
              (define-key js2-mode-map (kbd "C-c j s e") 'nodejs-repl-send-last-sexp)))
  (add-hook 'nodejs-repl-mode #'smartparens-mode))

(use-package js2-refactor :ensure t :defer t
  :commands js2-refactor-mode
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package json-snatcher :ensure t :defer t
  :after js2-mode
  :config
  (bind-key "C-c C-g" 'jsons-print-path js2-mode-map))

(use-package skewer-mode :ensure t :defer t :disabled t
  :diminish skewer-mode
  :init
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;; (use-package json-mode
;;   :ensure t
;;   :defer t
;;   :mode ("\\.json\\'" . json-mode))

;;; react-snippets, angular-mode + angular-snippets
(use-package react-snippets :ensure t :defer t)
(use-package angular-mode :ensure t :defer t)
(use-package angular-snippets :ensure t :defer t
  :config
  (eval-after-load "web-mode"
    '(bind-key "C-c C-d" 'ng-snip-show-docs-at-point web-mode-map)))

(use-package js-comint :ensure t :disabled t
  :config
  (progn
    (setq inferior-js-program-command "node")
    (setq inferior-js-program-arguments '("--interactive"))

    (add-hook 'inferior-js-mode-hook
              (lambda ()
                (ansi-color-for-comint-mode-on)))

    (defun inferior-js-mode-hook-setup ()
      (add-hook 'comint-output-filter-functions 'js-comint-process-output))
    (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)))

(provide 'config-javascript)
