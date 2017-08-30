;;; matlab
(use-package matlab-mode :ensure t :defer t :disabled t
  :mode ("\\.m\\'" . matlab-mode)
  :commands (matlab-mode matlab-shell))

(when (not (fboundp 'matlab-mode))
  (el-get-bundle cute-jumper/matlab-mode))

(add-to-list 'load-path (concat user-emacs-directory "el-get/matlab-mode/"))

(eval-after-load 'matlab
  '(progn
     (setq matlab-indent-function-body t
           matlab-functions-have-end nil
           matlab-vers-on-startup nil
           matlab-handle-simulink nil
           matlab-shell-ask-MATLAB-for-completions nil
           matlab-shell-command-switches '("-nosplash -nodesktop"))
     (matlab-functions-have-end-minor-mode nil)
     (setq matlab-shell-command
           (or
            (executable-find "matlab")
            (executable-find "/usr/local/bin/matlab")
            (executable-find "/Applications/MATLAB_R2014b.app/bin/matlab")))

     (add-hook 'matlab-mode-hook
               (lambda ()
                 (set (make-local-variable 'company-backends)
                      '(company-matlab
                        company-matlab-shell
                        company-files))
                 (company-mode +1)
                 (yas-minor-mode +1)
                 (smartparens-mode +1)))

     (add-hook 'matlab-shell-mode-hook
               (lambda()
                 (set (make-local-variable 'company-backends)
                      '(company-matlab
                        company-matlab-shell
                        company-files))
                 (company-mode +1)
                 (smartparens-mode +1)))
     ;;(add-hook 'matlab-mode-hook 'company-mode)
     ;;(add-hook 'matlab-mode-hook 'smartparens-mode)
     ;; (add-to-list 'company-backends 'company-matlab-shell)
     ))

(provide 'config-matlab)
