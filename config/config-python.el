;;; package --- Summary:

;;; Commentary:

;;; Code:

(use-package python :ensure t :defer t
  :commands (run-python)
  :mode ("\\.py\\'" . python-mode)
  ;; (remove-hook 'python-mode-hook #'python-setup-shell)
  :init
  (setq python-shell-completion-native-enable nil)
  (setenv "JUPYTER_CONSOLE_TEST" "1")
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1") 
  :config
  (setq mode-name "Python"
        tab-width 4
        fill-column 80
        comment-inline-offset 2
        python-indent-offset 4
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i")

  ;; python拓展功能包
  ;;  + anaconda-mode
  ;;  + company-anaconda
  (use-package company-anaconda :ensure t :defer t
    :init
    (add-to-list 'company-backends #'company-anaconda))
  (use-package anaconda-mode :ensure t :defer t
    :diminish anaconda-mode
    :init
    (setq anaconda-mode-installation-directory
          (expand-file-name ".cache/anaconda-mode" user-emacs-directory)))

  (defun python-send-line ()
    (interactive)
    (save-excursion 
      (back-to-indentation)
      (python-shell-send-string
       (concat (buffer-substring-no-properties
                (point)
                (line-end-position)) 
               "\n"))))
  
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
                     company-dabbrev))
              (smartparens-mode +1)))
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (indent-tabs-mode nil)
              (smartparens-mode t))))

(use-package pyvenv :ensure t :defer t)

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

