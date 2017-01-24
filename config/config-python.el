;; -----------------------------------
;; python-mode
;; -----------------------------------
(use-package python
  :ensure t
  :mode
  ("\\.py\\'" . python-mode)
  :init
  (progn
    (setq python-shell-interpreter "ipython2"
          python-shell-interpreter-args "--pylab"
          python-shell-buffer-name "Python"
          python-indent 4
          tab-width 4)
    (setq-local comment-inline-offset 2))
  :config
  (progn
    (add-hook 'python-mode-hook 'eldoc-mode)))

(use-package anaconda-mode
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)))

(use-package company-anaconda
  :ensure t
  :defer t
  :init
  (progn
    (add-to-list 'company-backends'company-anaconda)))

;; (use-package py-autopep8
;;   :ensure t
;;   :config
;;   (setq py-autopep8-options '("--max-line-length=100")))

(use-package elpy
  :ensure t
  :init
  (progn
    (setenv "WORKON_HOME" "$HOME/anaconda3/envs")
    (pyvenv-mode 1))
  :config
  (progn
    (setq elpy-rpc-backend "jedi")
    (elpy-enable)
    (elpy-use-ipython)
    (add-to-list 'company-backends 'elpy-company-backend)))

;; (defalias 'workon 'pyvenv-workon)
;; (defalias 'runpy 'run-python)
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

(use-package ein
  :ensure t
  :config
  (progn
    ;; Use Jedi with EIN
    ;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
    (setq ein:use-auto-complete t)
    (setq ein:default-url-or-port "http://localhost:9999"
          ein:output-type-perference '(emacs-lisp svg png jpeg
                                                  html text latex javascript))
    (use-package websocket :ensure t)))

;; (use-package ob-ipython
;;   :ensure t
;;   :init
;;   (setq org-babel-python-command "python2")
;;  ;;; --- goto: https://github.com/gregsexton/ob-ipython
;;   (setq org-confirm-babel-evaluate nil) ; don't prompt me to confirm
;;                                         ; everytime I want to evaluate a block
;;   ;;; display/update images in the buffer after I evaluate
;;   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
;;   ;;; ----------------------------------------
;;   )

(provide 'config-python)
;;; config-python.el ends here
