
;;; Must-have packages:
;; web-mode, emmet-mode, js2-mode, tern, rainbow-mode, jade

;; helper package
(use-package company-web :ensure t)
(use-package company-tern :ensure t)
(use-package web-beautify :ensure t :defer t)
(use-package lorem-ipsum :ensure t :defer t)
(use-package rainbow-mode :ensure t :defer t)
(use-package emmet-mode :ensure t
  :config
  (setq emmet-indentation 2
        emmet-move-cursor-between-quotes t
        emmet-move-cursor-after-expanding nil
        emmet-self-closing-tag-style " /"
        emmet-preview-default nil)
  (add-hook 'emmet-mode-hook
            (lambda ()
              (local-set-key (kbd "s-<left>") 'emmet-prev-edit-point)
              (local-set-key (kbd "s-<right>") 'emmet-next-edit-point))))

(use-package web-mode :ensure t
  :mode (("\\.html?$" . web-mode)
         ("\\.ejs$" . web-mode)
         ("\\.jsx$" . web-mode)
         ("\\.hbs$" . web-mode)
         ("\\.handlebars$" . web-mode)
         ("\\.gohtml$" . web-mode)
         ("\\.djhtml$" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing nil
        web-mode-enbale-auto-closing t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)

  (setq web-mode-engines-alist
        '(("erb" . "\\.ejs\\'")
          ("ctemplate" . "\\.hbs$")
          ("go" . "\\.gohtml$")
          ("php" . "\\.php$")))

  (add-hook 'web-mode-hook
            (lambda()
              ;; flycheck
              ;;(flycheck-mode t)
              ;;(flycheck-add-mode 'javascript-eslint 'web-mode)
              (setq indent-tabs-mode nil)
              (hl-line-mode t)
              (rainbow-mode t)
              (emmet-mode t)
              (smartparens-mode t)
              (yas-minor-mode t)))

  (define-key web-mode-map (kbd "C-'") 'company-web-html)

  ;; I want to use smartparens
  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))
  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    "Set `tern-mode' based on current language before running company-tern."
    (message "advice")
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language
               (web-mode-language-at-pos)))
          (if (or (string= web-mode-cur-language "javascript")
                  (string= web-mode-cur-language "jsx")
                  )
              (unless tern-mode (tern-mode))
            (if tern-mode (tern-mode -1))))))

  ;; set local company mode
  (add-hook 'web-mode-hook
            (lambda ()
              (add-to-list 'company-backends
                           '(company-web-html
                             company-tern)))))

(use-package js2-mode :ensure t :defer t
  :interpreter (("node" . js2-mode))
  :mode "\\.\\(js\\|json\\)$"
  :config
  (setq js2-indent-level 2
        js2-basic-offset 2
        js2-highlight-level 3
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  (add-hook 'js-mode-hook #'js2-minor-mode)
  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key "\C-c\C-p" 'js2-print-json-path)
              (local-set-key "\C-x\C-e" 'js2-send-last-sexp)
              (local-set-key "\C-\M-x" 'js2-send-last-sexp-and-go)
              (local-set-key "\C-cb" 'js2-send-buffer)
              (local-set-key "\C-c\C-b" 'js2-send-buffer-and-go)
              (local-set-key "\C-c\C-r" 'js2-send-region-and-go)
              (local-set-key "\C-cl" 'js2-load-file-and-go)
              (local-set-key "\C-c\C-z" 'run-js)
              ;; js2 ignores some commands
              (local-set-key (kbd "RET") 'newline-and-indent)
              (local-set-key "\C-a" 'back-to-indentation)
              (local-set-key (kbd "\C-c i") 'jslint-current-buffer)
              ;; minor mode
              (yas-minor-mode t)
              (tern-mode t)
              (hl-line-mode t)
              (smartparens-mode t)
              (indium-interaction-mode t)
              ))
  (add-hook 'js2-mode-hook
            (lambda ()
              ;; set local company-mode
              (add-to-list 'company-backends
                           '(company-tern)))))

(use-package indium :ensure t :defer t
  :config (add-hook 'js3-mode-hook #'indium-interaction-mode))
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
              (define-key js3-mode-map (kbd "C-c /") 'nodejs-repl-get-candidates)
              (define-key js3-mode-hook (kbd "C-c C-l") 'nodejs-repl-load-file)
              (define-key js3-mode-hook (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)
              (define-key js3-mode-hook (kbd "C-c s r") 'nodejs-repl-send-region)
              (define-key js3-mode-hook (kbd "C-c s e") 'nodejs-repl-send-last-sexp)))
  (add-hook 'nodejs-repl-mode #'smartparens-mode))

(use-package pug-mode :ensure t
  :mode (("\\.pug$" . pug-mode)
         ("\\.jade$" . pug-mode))
  :config
  (add-hook 'pug-mode-hook
            (lambda()
              (add-to-list 'company-backends
                           '(company-web-jade))))
  (add-hook 'pug-mode-hook
            (lambda()
              (hl-line-mode t)
              (yas-minor-mode t)
              (smartparens-mode t)))
  (add-hook 'after-save-hook #'pug-compile))

;; (use-package dumb-jump
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;;          ("M-g j" . dumb-jump-go)
;;          ("M-g x" . dumb-jump-go-prefer-external)
;;          ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
;;   :ensure)

(use-package js2-refactor :ensure t :defer t :disabled t
  :commands js2-refactor-mode
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package json-snatcher :ensure t :defer t :disabled t
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

;;; CSS
(use-package css-mode :ensure t :defer t
  :mode "\\.css$"
  :config
  (progn
    (add-hook 'css-mode-hook
              (lambda ()
                ;; set local company mode
                (set (make-local-variable 'company-backends)
                     '(company-css
                       company-yasnippet
                       company-files))
                ;; basic configuration
                (setq css-indent-offset 2)
                ;; Minor modes
                (rainbow-mode t)
                (emmet-mode t)
                (yas-minor-mode t)))

    (defun css-expand-statement ()
      "Expand CSS block"
      (interactive)
      (save-excursion
        (end-of-line)
        (search-backward "{")
        (forward-char 1)
        (while (or (eobp) (not (looking-at "}")))
          (let ((beg (point)))
            (newline)
            (search-forward ";")
            (indent-region beg (point))
            ))
        (newline)))

    (defun css-contract-statement ()
      "Contract CSS block"
      (interactive)
      (end-of-line)
      (search-backward "{")
      (while (not (looking-at "}"))
        (join-line -1)))))

;; (use-package sass-mode :ensure t :defer t
;;   :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode :ensure t :defer t
  :mode (("\\.scss\\'" . scss-mode)
         ("\\.sass\\'" . scss-mode))
  :config
  (add-hook 'scss-mode-hook #'rainbow-mode)
  (add-hook 'scss-mode-hook #'emmet-mode))

(use-package less-css-mode :ensure t :defer t
  :mode ("\\.less\\'" . less-css-mode)
  :config
  (setq less-css-compile-at-save t))

(use-package yaml-mode :ensure t :defer t
  :mode ("\\.ya?ml$" . yaml-mode))

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
    (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)
    )
  )


;; php mode
(use-package php-mode :ensure t :disabled t
  :mode ("\\.php\\'" . php-mode)
  :config
  (progn
    (use-package ac-php
      :ensure t
      :after php-mode)
    (use-package php-eldoc
      :ensure t
      :after php-mode)
    ))

;; (use-package company-php
;;   :ensure t
;;   :after php-mode)

(eval-after-load 'php-mode
  '(progn
     (add-hook 'php-mode-hook 'smartparens-mode)
     (add-hook 'php-mode-hook 'php-eldoc-enable)
     (add-hook 'php-mode-hook
               (lambda ()
                 (company-mode t)
                 (add-to-list 'company-backends 'company-ac-php-backend)))))

(use-package psysh :disabled t
  :if (executable-find "psysh")
  :ensure t)

(provide 'config-web-development)
;;; config-web.el ends here
