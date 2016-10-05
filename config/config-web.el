;; -----------------------------------
;; web-mode
;; -----------------------------------
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
   ("\\.djhtml\\'"     . web-Mode)))

(use-package company-web :ensure t)

(eval-after-load 'web-mode
  '(progn
     (add-to-list 'company-backends 'company-web-html)
     (define-key web-mode-map (kbd "C-c b") 'web-beautify-html)))

;; -----------------------------------
;; javascript
;; -----------------------------------
(use-package js2-mode
  :ensure t
  :defer t
  :mode ("\\.js$" . js2-mode))

(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json$" . json-mode))

(use-package css-mode
  :ensure t
  :mode ("\\.css$" . css-mode)
  :config (setq css-indent-offset 2))

(use-package scss-mode
  :ensure t
  :mode ("\\.scss\\'" . scss-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml$'" . yaml-mode))

(use-package web-beautify :ensure t)

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
;; (eval-after-load 'js
;;   '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
;; (eval-after-load 'sgml-mode
;;   '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(progn
     (define-key css-mode-map (kbd "C-c b") 'web-beautify-css)
     (add-to-list 'company-backends 'company-css)))

;; -----------------------------------
;; javascript REPL
;; -----------------------------------

;; (use-package nodejs-repl
;;   :ensure t
;;   :defer t)

(use-package js-comint
  :ensure t
  :init
  (progn
    (setq inferior-js-program-command "node")
    (setq inferior-js-program-arguments '("--interactive"))))

(eval-after-load 'js2-mode
  '(progn
     (setq js-indent-level 2)
     (add-hook 'js2-mode-hook '(lambda ()
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

;; -----------------------------------
;; php mode
;; -----------------------------------
(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))
(use-package company-php :ensure t)
(use-package ac-php :ensure t)
(use-package php-eldoc :ensure t)
(eval-after-load 'php-mode
  '(progn
     (add-hook 'php-mode-hook 'smartparens-mode)
     (add-hook 'php-mode-hook 'php-eldoc-enable)
     (add-hook 'php-mode-hook
               (lambda ()
                 (company-mode t)
                 (add-to-list 'company-backends 'company-ac-php-backend)))
     ))

;; -----------------------------------
;; jade
;; -----------------------------------
(use-package pug-mode
  :ensure t
  :mode (("\\.pug\\'" . pug-mode)
         ("\\.jade\\'" . pug-mode)))

;; (use-package jade
;;   :ensure t)
;; (eval-after-load 'js2-mode
;;   '(add-hook 'js2-mode-hook #'jade-interaction-mode))

;; -----------------------------------
;; php REPL
;; -----------------------------------
(use-package psysh
  :if (executable-find "psysh")
  :ensure t)

(provide 'config-web)
;;; config-web.el ends here
