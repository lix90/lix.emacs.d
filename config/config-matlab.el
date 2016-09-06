;; -------------------------
;; matlab
;; -------------------------
(use-package matlab-mode
  :ensure t
  :mode ("\\.m$" . matlab-mode)
  :commands (matlab-mode matlab-shell)
  :bind (:map matlab-mode-map
              ("C-M-c" . matlab-shell-run-cell)
              ("C-M-r" . matlab-shell-run-region))
  :config (progn
            (setq matlab-indent-function-body t)
            (setq matlab-shell-command
                  (or
                   (executable-find "matlab")
                   (executable-find "/usr/local/bin/matlab")
                   (executable-find "/Applications/MATLAB_R2012b.app/bin/matlab")))
            (setq matlab-shell-command-switches '("-nosplash -nodesktop -nojvm"))
            (add-hook 'matlab-mode-hook 'company-mode)
            (add-hook 'matlab-mode-hook 'smartparens-mode)))

(provide 'config-matlab)

;;; config-matlab.el ends here
