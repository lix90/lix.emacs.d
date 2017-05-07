;; helper package
(use-package company-web :ensure t :defer t)

(use-package company-tern :ensure t :defer t)

(use-package web-beautify :ensure t :defer t)

(use-package lorem-ipsum :ensure t :defer t)

(use-package rainbow-mode :ensure t :defer t)

(use-package emmet-mode :ensure t :defer t
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

(use-package web-mode :ensure t :defer t
  :mode (("\\.html?$" . web-mode)
         ("\\.ejs$" . web-mode)
         ("\\.jsx$" . web-mode)
         ("\\.hbs$" . web-mode)
         ("\\.handlebars$" . web-mode)
         ("\\.gohtml$" . web-mode)
         ("\\.djhtml$" . web-mode)
         ("\\.vue$" . web-mode)
         )
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing nil
        web-mode-enbale-auto-closing t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-script-padding 4
        web-mode-style-padding 4)

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

(use-package pug-mode :ensure t
  :mode (("\\.pug$" . pug-mode)
         ("\\.jade$" . pug-mode))
  :config
  (add-hook 'pug-mode-hook
            (lambda()
              (add-to-list 'company-backends
                           '(company-web-jade))))
  ;; (add-hook 'pug-mode-hook
  ;;           (lambda()
  ;;             ;;(hl-line-mode t)
  ;;             (yas-minor-mode t)
  ;;             (smartparens-mode t)))
  ;;(add-hook 'after-save-hook #'pug-compile)
  )

;; (use-package dumb-jump
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;;          ("M-g j" . dumb-jump-go)
;;          ("M-g x" . dumb-jump-go-prefer-external)
;;          ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
;;   :ensure)

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

;;; Vue mode
(use-package vue-mode :ensure t :defer t :disabled t
  :mode (("\\.vue$" . vue-mode))
  :config
  (setq js-indent-level 2)
  (setq mmm-submode-decoration-level 0))

(provide 'config-web)
;;; config-web.el ends here
