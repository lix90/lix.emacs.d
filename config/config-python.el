;; python-mode
;; -----------------------------------
;; -----------------------------------

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  )
(use-package anaconda-mode
  :ensure t)

(use-package company-anaconda
  :ensure t)

(use-package py-autopep8
  :ensure t
  :config (progn
            (setq py-autopep8-options '("--max-line-length=100"))
            )
  )

(use-package elpy
  :ensure t
  :diminish elpy-mode
  :config (progn
            (setq elpy-rpc-backend "jedi")
            (elpy-enable)
            (elpy-use-ipython)
            (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
            )
  )

(eval-after-load 'python
  '(progn
     (add-hook 'python-mode-hook 'anaconda-mode)
     (add-hook 'python-mode-hook 'eldoc-mode)
     (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
     (add-to-list 'company-backends 'company-anaconda)
     )
  )

;; (use-package ac-anaconda
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook 'ac-anaconda-setup))
;; (use-package helm-pydoc
;;   :ensure t
;;   :defer t)

;; (use-package ein
;;   :ensure t
;;   :config (progn
;;             ;; Use Jedi with EIN
;;             ;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
;;             (setq ein:use-auto-complete t)
;;             (setq ein:default-url-or-port "http://localhost:8888"
;;                   ein:output-type-perference '(emacs-lisp svg png jpeg
;;                                                           html text latex javascript))
;;             (use-package websocket :ensure t)))

;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (defun my/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'my/python-mode-hook)
;;   )
(eval-after-load 'python-mode
  '(setq python-indent-offset 4))

(provide 'config-python)
;;; config-python.el ends here
