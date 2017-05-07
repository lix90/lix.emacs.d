;; helper packages
(use-package ess-smart-underscore :ensure t :defer t :disabled t)
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
        ess-ask-for-ess-directory nil
        ess-eval-visibly nil
        ess-directory user-project-directory
        ;; Keep global .Rhistory file.
        ess-history-directory "~/.R/"
        inferior-R-args "-q" ; I donnot want to print startup message
        ess-use-company t
        ess-use-auto-complete nil
        ess-eldoc-show-on-symbol t
        eldoc-echo-area-use-multiline-p t
        ido-enable-flex-matching t
        )
  (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
  (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)
  ;;(define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)

  (add-hook 'ess-mode-hook
            (lambda()
              (smartparens-mode t)
              (yas-minor-mode t)))
  (add-hook 'inferior-ess-mode-hook
            (lambda()
              (smartparens-mode +1)
              (global-hl-line-mode -1)
              (global-linum-mode -1)
              (fringe-mode '(8 . 2))))

  (defun ess-browse-vignette ()
    (interactive)
    (setq R-cmd (format "browseVignettes(\"%s\")"
                        (read-string "Enter package name:")))
    (ess-execute R-cmd 'buffer nil nil))
  (define-key inferior-ess-mode-map (kbd "C-c C-d C-v") 'ess-browse-vignette)

  (defun ess-browse-help ()
    (interactive)
    (setq R-cmd (format "?%s"
                        (read-string "Enter object name:")))
    (ess-execute R-cmd 'buffer nil nil))
  (define-key inferior-ess-mode-map (kbd "C-c C-d C-o") 'ess-browse-help)

  (use-package key-combo :ensure t :defer t
    :init
    (add-hook 'ess-mode-hook
              '(lambda()
                 (key-combo-mode t)))
    (add-hook 'inferior-ess-mode-hook
              '(lambda()
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



(provide 'config-ess-r)
