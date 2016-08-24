;; -------------------------
;; matlab
;; -------------------------
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
                   (executable-find "/Applications/MATLAB_R2012b.app/bin/matlab")))
            (add-hook 'matlab-mode-hook 'company-mode)))

(provide 'config-matlab)

;;; config-matlab.el ends here
