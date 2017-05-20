;;; Python configuration
;; helper packages

(use-package anaconda-mode :ensure t :defer t)

(use-package company-anaconda :ensure t :defer t)

(use-package elpy :ensure t :disabled t
  :init (add-to-list 'company-backends 'elpy-company-backend))

(use-package virtualenvwrapper :ensure t :defer t
  :init (venv-initialize-eshell)
  :config
  (venv-initialize-eshell)
  (setq venv-location (concat (getenv "HOME") "/anaconda3/envs/")))

(use-package pyvenv :ensure t :defer t
  :init
  (setenv "WORKON_HOME" (concat (getenv "HOME") "/anaconda3/envs/"))
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (defconst python-version "python3")
  (defun lix/echo-python-version()
    (interactive)
    (message (shell-command-to-string "python --version")))
  (defun lix/switch-to-python2 ()
    (interactive)
    (pyvenv-workon (if is-mac "python2" "py27"))
    (message "Python2 is running!"))
  (defun lix/switch-to-python3 ()
    (interactive)
    (pyvenv-workon "..")
    (message "Python3 is running!"))
  (defun run-python2()
    (interactive)
    (lix/switch-to-python2)
    (run-python))
  (defun run-python3()
    (interactive)
    (lix/switch-to-python3)
    (run-python)))

(use-package python :ensure t :commands (run-python)
  :mode ("\\.py\\'" . python-mode)
  ;; (remove-hook 'python-mode-hook #'python-setup-shell)
  :config
  (defun python-send-line ()
    (interactive)
    (save-excursion 
      (back-to-indentation)
      (python-shell-send-string
       (concat (buffer-substring-no-properties
                (point)
                (line-end-position)) 
               "\n"))))
  (setq mode-name "Python"
        tab-width 4
        fill-column 80
        comment-inline-offset 2
        python-indent-offset 4
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i")
  (when (version< emacs-version "24.5")
    ;; auto-indent on colon doesn't work well with if statement
    ;; should be fixed in 24.5 and above
    (setq electric-indent-chars (delq ?: electric-indent-chars)))
  
  (add-hook 'python-mode-hook
            (lambda()
              (flycheck-mode t)
              (anaconda-mode t)
              (anaconda-eldoc-mode t)
              (electric-indent-mode t)
              ;; key-bindings
              (local-set-key (kbd "C-j") #'newline-and-indent)
              (local-set-key (kbd "C-c C-r") #'python-shell-send-region)
              (local-set-key (kbd "C-c C-d") #'python-shell-send-defun)
              (local-set-key (kbd "s-<return>") (lambda ()
                                                  (interactive)
                                                  (python-send-line)
                                                  (next-line)))))
  ;; set local company-mode
  (add-hook 'python-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-anaconda company-dabbrev-code)
                     company-dabbrev))))
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (setq indent-tabs-mode t)
              (smartparens-mode t))))

;; (use-package ein :ensure t :defer t
;;   :config
;;   (progn
;;     ;; Use Jedi with EIN
;;     ;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup) 
;;     (setq ein:use-auto-complete t 
;;           ein:default-url-or-port "http://localhost:8888"
;;           ein:query-timeout 1000
;;           ein:output-type-perference '(emacs-lisp
;;                                        svg png
;;                                        jpeg html
;;                                        text latex
;;                                        javascript)
;;           ein:console-args
;;           (if is-mac '("--gui=osx" "--matplotlib=osx" "--colors=Linux")))
;;     (use-package websocket :ensure t)))

(provide 'config-python)
;;; config-python.el ends here

