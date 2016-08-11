;;; Commentary:
;;; package --- Summary

;;; Code:
;;useR
(use-package ess-site
  :ensure ess
  :mode (("/R/.*\\.q\\'"       . R-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.Rd\\'"           . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :commands R
  :config (progn
            (use-package company
              :ensure t
              :init
              (add-hook 'ess-mode-hook 'company-mode))
            (use-package flycheck
              :ensure t
              :config (progn
                        (setq flycheck-lintr-linters "with_defaults(camel_case_linter = NULL, snake_case_linter, object_usage_linter = NULL, commented_code_linter = NULL, infix_spaces_linter = NULL, spaces_left_parentheses_linter = NULL, line_length_linter(120))"))))
  )

(with-eval-after-load 'ess-site
  ;; Follow Hadley Wickham's R style guide
  (setq ess-first-continued-statement-offset 2
        ess-continued-statement-offset 0
        ess-expression-offset 2
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'DEFAULT
        ess-ask-for-ess-directory nil
        ess-eval-visibly nil
        ;; Keep global .Rhistory file.
        ess-history-directory "~/.R/")
  (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
  (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
  (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input))



;; matlab
(use-package matlab-mode
  :ensure t
  :mode ("\\.m$" . matlab-mode)
  :commands (matlab-mode matlab-shell)
  :config (progn
            (setq matlab-indent-function-body t)
            (setq matlab-shell-command
                  (or
                   (executable-find "matlab")
                   (executable-find "/usr/local/bin/matlab")
                   (executable-find "/Applications/MATLAB_R2012b.app/bin/matlab")
                   )
                  )
            (use-package company
              :ensure t
              :init
              (add-hook 'matlab-mode-hook 'company-mode))
            )
  )

(defalias 'run-matlab 'matlab-shell)


;; markdown
(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.text\\'" . markdown-mode))
  :config (progn
            (add-hook 'markdown-mode-hook 'visual-line-mode)
            (use-package markdown-toc
              :ensure t)
            )
  )

;;======================================
;; python-mode
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config (progn
            (use-package elpy
              :ensure t
              :init (setq elpy-rpc-backend "jedi")
              :config (progn
                        (elpy-enable)
                        (elpy-use-ipython)))

            (use-package anaconda-mode
              :ensure t
              :commands anaconda-mode
              :diminish anaconda-mode
              :config
              (progn
                (add-hook 'python-mode-hook 'anaconda-mode)
                (add-hook 'python-mode-hook 'eldoc-mode)))

            (use-package company-anaconda
              :ensure t
              :init
              (eval-after-load "company"
                '(add-to-list 'company-backends 'company-anaconda)))

            (use-package ac-anaconda
              :ensure t
              :init
              (add-hook 'python-mode-hook 'ac-anaconda-setup))
            )
  )

;; (use-package helm-pydoc
;;   :ensure t
;;   :defer t)

(use-package ein
  :ensure t
  :config (progn
            ;; Use Jedi with EIN
            ;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
            (setq ein:use-auto-complete t)
            (setq ein:default-url-or-port "http://localhost:8888"
                  ein:output-type-perference '(emacs-lisp svg png jpeg
                                                          html text latex javascript))
            (require 'websocket)
            )
  )

;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (defun my/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'my/python-mode-hook)
;;   )

;; web-mode

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
   ("\\.djhtml\\'"     . web-mode)))

;; javascript
(use-package js2-mode
  :ensure t
  :defer t
  :mode ("\\.js" . js2-mode))

(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json" . json-mode))

(use-package nodejs-repl
  :ensure t
  :defer t)

(use-package css-mode
  :ensure t
  :mode ("\\.css$" . css-mode))

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
      '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
    )
  )


(provide 'config-lang)

;;; config-lang.el ends here
