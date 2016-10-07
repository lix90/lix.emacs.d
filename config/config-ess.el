;;; useR --- ess
;;; Commentary:
;;; Code:

(use-package ess-site
  :ensure ess
  :mode (("/R/.*\\.q\\'"       . R-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.Rd\\'"           . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :commands R
  )


(with-eval-after-load 'ess-site
  ;; Follow Hadley Wickham's R style guide
  (setq ess-first-continued-statement-offset 2
        ess-continued-statement-offset 0
        ess-expression-offset 2
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'DEFAULT
        ess-ask-for-ess-directory nil
        ess-eval-visibly nil
        ;; Keep global .Rhistory file.
        ess-history-directory "~/.R/")
  (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
  (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
  (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)
  (add-hook 'inferior-ess-mode-hook 'company-mode)
  (add-hook 'inferior-ess-mode-hook 'smartparens-mode))

(provide 'config-ess)

;;; config-ess.el ends here
