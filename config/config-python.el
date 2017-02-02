;; -----------------------------------
;; python-mode
;; -----------------------------------
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode))

;; (use-package py-autopep8
;;   :ensure t
;;   :config
;;   (setq py-autopep8-options '("--max-line-length=100")))

;; (defalias 'workon 'pyvenv-workon)
;; (defalias 'runpy 'run-python)


(eval-after-load 'python
  '(progn

     (remove-hook 'python-mode-hook #'python-setup-shell)


     (setq mode-name "Python"
           tab-width 4
           python-shell-interpreter "ipython2"
           python-shell-interpreter-args "--pylab")
     (when (version< emacs-version "24.5")
       ;; auto-indent on colon doesn't work well with if statement
       ;; should be fixed in 24.5 and above
       (setq electric-indent-chars (delq ?: electric-indent-chars)))
     (setq-local comment-inline-offset 2)
     ;; make C-j work the same way as RET
     (local-set-key (kbd "C-j") 'newline-and-indent)
     (setenv "IPY_TEST_SIMPLE_PROMPT" "1")

     (defun inferior-python-setup-hook ()
       (setq indent-tabs-mode t))

     (defun eldoc-python-mode ()
       (eldoc-mode)
       (anaconda-eldoc-mode))

     (defun elpy-default()
       (setq elpy-rpc-backend "jedi")
       (elpy-enable)
       (elpy-use-ipython)
       (add-to-list 'company-backends 'elpy-company-backend)
       (local-set-key (kbd "C-c C-u") 'elpy-flymake-show-error))

     (add-hook 'inferior-python-mode-hook #'inferior-python-setup-hook)
     ;;(add-hook 'inferior-python-mode-hook #'python-default)
     ;;(add-hook 'python-mode-hook #'python-default)
     (add-hook 'python-mode-hook #'eldoc-python-mode)
     (add-hook 'python-mode-hook 'eldoc-mode)
     (add-hook 'python-mode-hook #'elpy-default)
     (add-hook 'python-mode-hook 'anaconda-mode)
     (add-to-list 'company-backends 'company-anaconda)

     (use-package elpy
       :ensure t)

     (use-package anaconda-mode
       :ensure t
       :defer t)

     (use-package company-anaconda
       :ensure t
       :defer t)))

;; (use-package ein
;;   :ensure t
;;   :config
;;   (progn
;;     ;; Use Jedi with EIN
;;     ;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
;;     (setq ein:use-auto-complete t)
;;     (setq ein:default-url-or-port "http://localhost:8888"
;;           ein:output-type-perference '(emacs-lisp
;;                                        svg png
;;                                        jpeg html
;;                                        text latex
;;                                        javascript))
;;     (use-package websocket :ensure t)))

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
