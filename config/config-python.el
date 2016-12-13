;; -----------------------------------
;; python-mode
;; -----------------------------------

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config
  (progn
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "--pylab")))

;; (use-package anaconda-mode
;;   :ensure t
;;   :defer t)

;; (use-package company-anaconda
;;   :ensure t
;;   :defer t)

(use-package py-autopep8
  :ensure t
  :defer t
  :config
  (setq py-autopep8-options '("--max-line-length=100")))

(use-package elpy
  :ensure t
  :defer t
  :diminish elpy-mode
  :config
  (setq elpy-rpc-backend "jedi")
  (elpy-enable)
  (elpy-use-ipython)
  (add-to-list 'company-backends 'elpy-company-backend))

(eval-after-load 'python-mode
  '(progn
     (setq python-indent-offset 4)
     ;; (add-hook 'python-mode-hook 'anaconda-mode)
     (add-hook 'python-mode-hook 'eldoc-mode)
     (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
     (add-to-list 'company-backends 'company-anaconda)))
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

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


(provide 'config-python)
;;; config-python.el ends here
