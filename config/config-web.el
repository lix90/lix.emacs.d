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
   ("\\.djhtml\\'"     . web-mode))
  :config (progn
            (use-package company-web
              :ensure t
              :init
              (progn (require 'company-web-html)))))

(eval-after-load 'company-web
  '(progn
     (add-hook 'web-mode-hook
               (lambda ()
                 (set
                  (make-local-variable 'company-backends) '(company-web-html))
                 (company-mode t)))))

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

(use-package nodejs-repl
  :ensure t
  :defer t)

(use-package css-mode
  :ensure t
  :mode ("\\.css$" . css-mode)
  :config (setq css-indent-offset 2))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml$'" . yaml-mode))

(use-package web-beautify
  :ensure t
  :init
  (progn
    (eval-after-load 'js2-mode
      '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
    ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
    ;; (eval-after-load 'js
    ;;   '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

    (eval-after-load 'json-mode
      '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

    ;; (eval-after-load 'sgml-mode
    ;;   '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

    (eval-after-load 'web-mode
      '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

    (eval-after-load 'css-mode
      '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))))

;; javascript REPL
(use-package js-comint
  :ensure t
  :config
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


(provide 'config-web)

;;; config-web.el ends here
