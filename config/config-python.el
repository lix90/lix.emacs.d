;; -----------------------------------
;; python-mode
;; -----------------------------------
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
              (add-hook 'python-mode-hook 'ac-anaconda-setup))))

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
            (require 'websocket)))

;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (defun my/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'my/python-mode-hook)
;;   )

(provide 'config-python)

;;; config-python.el ends here
