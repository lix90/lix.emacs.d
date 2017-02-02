;;; useR --- ess
;;; Commentary:
;;; Code:

(use-package ess-site
  :ensure ess
  :mode
  (("\\.sp\\'"           . S-mode)
   ("/R/.*\\.q\\'"       . R-mode)
   ("\\.[qsS]\\'"        . S-mode)
   ("\\.ssc\\'"          . S-mode)
   ("\\.SSC\\'"          . S-mode)
   ("\\.[rR]\\'"         . R-mode)
   ("\\.[rR]nw\\'"       . Rnw-mode)
   ("\\.[sS]nw\\'"       . Snw-mode)
   ("\\.[rR]profile\\'"  . R-mode)
   ("NAMESPACE\\'"       . R-mode)
   ("CITATION\\'"        . R-mode)
   ("\\.omg\\'"          . omegahat-mode)
   ("\\.hat\\'"          . omegahat-mode)
   ("\\.lsp\\'"          . XLS-mode)
   ("\\.do\\'"           . STA-mode)
   ("\\.ado\\'"          . STA-mode)
   ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
   ("\\.jl\\'"           . ess-julia-mode)
   ("\\.[Ss]t\\'"        . S-transcript-mode)
   ("\\.Sout"            . S-transcript-mode)
   ("\\.[Rr]out"         . R-transcript-mode)
   ("\\.Rd\\'"           . Rd-mode)
   ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
   ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
   ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
   ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
   ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
   ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :commands R
  :config
  (progn
    (setq ess-first-continued-statement-offset 2
          ess-continued-statement-offset 0
          ess-expression-offset 2
          ess-nuke-trailing-whitespace-p t
          ess-default-style 'DEFAULT
          ess-ask-for-ess-directory nil
          ess-eval-visibly nil
          ;; Keep global .Rhistory file.
          ess-history-directory "~/.R/"
          inferior-R-args "-q" ; I donnot want to print startup message
          )
    (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
    (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
    (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)

    ))


(eval-after-load 'ess-site
  '(progn
     (add-hook 'ess-mode-hook 'company-mode)
     (add-hook 'ess-mode-hook 'smartparens-mode)

     (use-package key-combo
       :ensure t
       :init
       (progn
         (key-combo-mode 1)
         (add-hook 'ess-mode-hook
                   '(lambda()
                      (key-combo-mode t)))
         (add-hook 'inferior-ess-mode-hook
                   '(lambda()
                      (key-combo-mode t)))
         (defvar key-combo-ess-default
           '((">"  . (" > " " %>% "))
             ("$"  . ("$" " %$% "))
             ("<>" . " %<>% ")
             ("*"  . ("*" " * " "%*%"))
             ("%" . ("%" "%in%" "%%"))
             ("^"  . ("^" " ^ "))
             ("/"  . ("/" " / "))
             ("=" . ("=" " = " " == "))
             ("!=" . (" != "))
             ("," . ("," ", "))
             ("~" . " ~ ")
             (":" . (":" "::" ":::"))
             (":="  . " := ") ; data.table
             ("->"  . " -> ")))
         (key-combo-define-hook '(ess-mode-hook inferior-ess-mode-hook)
                                'ess-key-combo-load-default
                                key-combo-ess-default)))))


;; Rmd in emacs
;; reference: http://futurismo.biz/archives/2982
(use-package polymode
  :ensure t
  :mode (("\\.[SR]nw\\'" . poly-noweb+r-mode)
         ("\\.Rmd\\'" . Rmd-mode))
  :init
  (defun Rmd-mode ()
    "ESS Markdown mode for Rmd files."
    (interactive)
    (require 'poly-R)
    (require 'poly-markdown)
    (R-mode)
    (yaml-mode)
    (poly-markdown+r-mode)))

(defun ess-rmarkdown-to-html ()
  (interactive)
  "Run kintr::knit2html on the current file."
  "https://gist.github.com/kohske/9128031"
  (shell-command
   (format "Rscript -e \"kintr::knit2html ('%s')\""
           (shell-quote-argument (buffer-file-name)))))

;; do this in R process
;; library (rmarkdown); render ("file_name.Rmd")
(defun ess-rmarkdown ()
  (interactive)
  "Compile R markdown (.Rmd). Should work for any output type."
  "http://roughtheory.com/posts/ess-rmarkdown.html"
                                        ; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (save-excursion
    (let* ((sprocess (ess-get-process ess-current-process-name))
           (sbuffer (process-buffer sprocess))
           (buf-coding (symbol-name buffer-file-coding-system))
           (R-cmd
            (format "library (rmarkdown); rmarkdown::render(\"%s\")"
                    buffer-file-name))
           (message "Running rmarkdown on %s" buffer-file-name)
           (ess-execute R-cmd 'buffer nil nil)
           (switch-to-buffer rmd-buf)
           (ess-show-buffer (buffer-name-sbuffer) nil)))))

;; (define-key polymode-mode-map "\M-ns" 'ess-rmarkdown)

(provide 'config-ess)
;;; config-ess.el ends here
