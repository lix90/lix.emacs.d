;; helper packages
(use-package ess-site :ensure ess :defer t
  :mode (("\\.sp\\'"           . S-mode)
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
  (setq ess-first-continued-statement-offset 2
        ess-continued-statement-offset '(cascade . 2)
        ess-expression-offset 2
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'DEFAULT
        ess-ask-for-ess-directory t
        ess-eval-visibly nil
        ;; Keep global .Rhistory file.
        ess-history-directory "~/.R/"
        inferior-R-args "-q" ; I donnot want to print startup message
        ess-use-company t
        ess-use-auto-complete nil
        ess-eldoc-show-on-symbol t
        eldoc-echo-area-use-multiline-p nil
        ido-enable-flex-matching t
        )
  (bind-key "C-j" 'comint-next-input inferior-ess-mode-map)
  (bind-key "C-k" 'comint-previous-input inferior-ess-mode-map)
  ;;(define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)

  (add-hook 'ess-mode-hook
            (lambda()
              (smartparens-mode t)
              (yas-minor-mode t)
              (company-mode t)))
  (add-hook 'inferior-ess-mode-hook
            (lambda()
              (smartparens-mode t)
              (global-hl-line-mode -1)
              (global-linum-mode -1)
              (fringe-mode '(8 . 2))
              (company-mode t)))

  (defun ess-browse-vignette ()
    (interactive)
    (setq R-cmd (format "browseVignettes(\"%s\")"
                        (read-string "Enter package name:")))
    (ess-execute R-cmd 'buffer nil nil))
  (bind-key "C-c C-d C-v" 'ess-browse-vignette inferior-ess-mode-map)

  (defun ess-browse-help ()
    (interactive)
    (setq R-cmd (format "?%s"
                        (read-string "Enter object name:")))
    (ess-execute R-cmd 'buffer nil nil))
  (define-key "C-c C-d C-o" 'ess-browse-help inferior-ess-mode-map)

  (use-package key-combo :ensure t :defer t :after ess
    :init
    (add-hook 'ess-mode-hook
              (lambda()
                (key-combo-mode t)))
    (add-hook 'inferior-ess-mode-hook
              (lambda()
                (key-combo-mode t)))
    (defvar key-combo-ess-default
      '((">"  . (">" " %>% " " %>>% "))
        ("<"  . ("<" " %<>% "))
        ("%" . ("%" " %in% " "%%"))
        ("$"  . ("$" " %$% "))
        ("*"  . ("*" "%*%"))
        ("=" . ("=" " = " " == "))
        (":" . (":" "::" ":::"))
        (":=" . " := ") ; data.table
        ("-"  . ("-" " -> "))
        ("_"  . (" <- " "_"))))
    (key-combo-define-hook
     '(ess-mode-hook inferior-ess-mode-hook)
     'ess-key-combo-load-default key-combo-ess-default)))

;;; Rmd
;;;============================================================
;;; reference: http://futurismo.biz/archives/2982
(use-package polymode :ensure t :after ess
  :mode (("\\.[SR]nw$" . poly-noweb+r-mode)
         ("\\.Rmd$" . Rmd-mode)
         ("\\.Rpres$" . Rmd-mode)
         ;;("\\.md$" . poly-markdown-mode)
         )
  :init
  (defun Rmd-mode ()
    "ESS Markdown mode for Rmd files."
    (interactive)
    ;; (setq load-path
    ;;       (append (list "path/to/polymode/" "path/to/polymode/modes/")
    ;;               load-path))
    (require 'poly-R)
    (require 'poly-markdown)
    (R-mode)
    (yaml-mode)
    (poly-markdown+r-mode))
  :config
  ;;(require 'polymode)
  ;;(require 'poly-markdown)
  (defun rmarkdown-render ()
    "Reformat the current (presumed) markdown formatted buffer into
another format (i.e. html) by running rmarkdown::render() on
it (which in turn calls pandoc and friends).

Display trace or error results in buffer *rmarkdown-render Output*

When underlying file is remote (i.e. tramp), perform conversion
on remote host (where Rscript must be on path and rmarkdown must
be installed/configured (i.e. including pandoc)).
"
    ;; https://github.com/vspinu/polymode/issues/30
    ;; TODO: recover when assumptions not met (e.g. RStudio offers to
    ;; install rmarkdown if needed).
    (interactive)
    (let ((render-command
           (read-string "render command:"
                        (format "render('%s',%s);"
                                (shell-quote-argument
                                 (file-name-nondirectory (buffer-file-name)))
                                "'all'"))))
      (get-buffer-create "*rmarkdown-render Output*")
      (start-file-process
       "rmarkdown-render" "*rmarkdown-render Output*"
       "Rscript"
       "-e" (message "withCallingHandlers({library(rmarkdown); %s}, clean=FALSE, error = function(e) {print(sys.calls())})"
                     render-command))))
  )

(provide 'config-ess-r)
;;; config-ess-r.el ends here
