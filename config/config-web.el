(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.twig\\'"       . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode)
   ("\\.[gj]sp\\'"     . web-mode)
   ("\\.as[cp]x?\\'"   . web-mode)
   ("\\.eex\\'"        . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.handlebars\\'" . web-mode)
   ("\\.hbs\\'"        . web-mode)
   ("\\.eco\\'"        . web-mode)
   ("\\.ejs\\'"        . web-mode)
   ("\\.djhtml\\'"     . web-Mode)
   ("\\.vue\\'"        . web-mode))
  :config
  (progn
    (defun lix--web-company-mode ()
      (set (make-local-variable 'company-backends)
           '(company-web-html
             company-files
             company-css
             company-web-jade
             company-web-slim))
      (company-mode t))

    (defun lix--web-mode-indent-style ()
      "Indent-style for web mode."
      (setq web-mode-markup-indent-offset 2
            web-mode-css-indent-offset 2
            web-mode-code-indent-offset 2
            web-mode-indent-style 2)
      (setq-default indent-tabs-mode nil))

    (add-hook 'web-mode-hook 'lix--web-company-mode)
    (add-hook 'web-mode-hook 'lix--web-mode-indent-style)))

(use-package company-web
  :ensure t)

(use-package lorem-ipsum
  :ensure t
  :defer t)

(use-package js2-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    (setq js-indent-level 2)
    (add-hook 'js2-mode-hook
              (lambda ()
                (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                (local-set-key "\C-cb" 'js-send-buffer)
                (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                (local-set-key "\C-c\C-r" 'js-send-region-and-go)
                (local-set-key "\C-cl" 'js-load-file-and-go)
                (local-set-key "\C-c\C-z" 'run-js)
                ;; js2 ignores some commands
                (local-set-key (kbd "RET") 'newline-and-indent)
                (local-set-key "\C-a" 'back-to-indentation)
                (local-set-key (kbd "\C-c i") 'jslint-current-buffer)))))

(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json\\'" . json-mode))

(use-package css-mode
  :ensure t
  :mode ("\\.css$" . css-mode)
  :config
  (progn
    (setq css-indent-offset 2)
    (add-to-list 'company-backends 'company-css)

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

(use-package sass-mode
  :defer t
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :ensure t
  :mode (("\\.scss\\'" . scss-mode)
         ("\\.sass\\'" . scss-mode)))

(use-package less-css-mode
  :defer t
  :mode ("\\.less\\'" . less-css-mode))

(use-package pug-mode
  :defer t
  :mode ("\\.pug$" . pug-mode))

;; (use-package vue-mode
;;   :ensure t
;;   :mode ("\\.vue\\'" . vue-mode))

;; (use-package mmm-mode
;; :ensure t)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :init
  (add-hook 'css-mode-hook #'rainbow-mode)
  (add-hook 'scss-mode-hook #'rainbow-mode)
  (add-hook 'web-mode-hook #'rainbow-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package web-beautify
  :ensure t)

(use-package nodejs-repl
  :ensure t)

(use-package js-comint
  :ensure t
  :config
  (setq inferior-js-program-command "node")
  (setq inferior-js-program-arguments '("--interactive")))
;; (defun inferior-js-mode-hook-setup ()
;;   (add-hook 'comint-output-filter-functions 'js-comint-process-output))
;; (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

;; php mode
(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))
;; (use-package company-php
;;   :ensure t
;;   :after php-mode)
(use-package ac-php
  :ensure t
  :after php-mode)
(use-package php-eldoc
  :ensure t
  :after php-mode)

(eval-after-load 'php-mode
  '(progn
     (add-hook 'php-mode-hook 'smartparens-mode)
     (add-hook 'php-mode-hook 'php-eldoc-enable)
     (add-hook 'php-mode-hook
               (lambda ()
                 (company-mode t)
                 (add-to-list 'company-backends 'company-ac-php-backend)))))

(use-package psysh
  :if (executable-find "psysh")
  :ensure t)

(use-package pug-mode
  :ensure t
  :mode (("\\.pug\\'" . pug-mode)
         ("\\.jade\\'" . pug-mode))
  :config
  (add-to-list 'company-backends 'company-web-jade))

(use-package emmet-mode
  :ensure t
  :diminish emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'sass-mode-hook 'emmet-mode)
    (add-hook 'scss-mode-hook 'emmet-mode)
    (setq emmet-move-cursor-between-quotes t
          emmet-move-cursor-after-expanding nil
          emmet-self-closing-tag-style " /")))

(provide 'config-web)
;;; config-web.el ends here
