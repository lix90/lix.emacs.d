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

  (add-hook 'ess-mode-hook 'smartparens-mode)
  (add-hook 'ess-mode-hook 'yas-minor-mode)
  (add-hook 'inferior-ess-mode-hook 'smartparens-mode)

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

;; Rmd in emacs
;; reference: http://futurismo.biz/archives/2982
(use-package polymode :ensure t
  :mode (("\\.[SR]nw\\'" . poly-noweb+r-mode)
         ("\\.Rmd\\'" . Rmd-mode))
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
                                "'all'"
                                ))))
      (get-buffer-create "*rmarkdown-render Output*")
      (start-file-process
       "rmarkdown-render" "*rmarkdown-render Output*"
       "Rscript"
       "-e" (message "withCallingHandlers({library(rmarkdown); %s}, clean=FALSE, error = function(e) {print(sys.calls())})"
                     render-command)
       )))
  (define-key polymode-mode-map "\M-ns" 'rmarkdown-render))

;;; Python configuration
;; helper packages
(use-package anaconda-mode :ensure t :defer t) 
(use-package company-anaconda :ensure t :defer t)
(use-package elpy :ensure t :disabled t
  :init (add-to-list 'company-backends 'elpy-company-backend))
(use-package pyvenv :ensure t :defer t
  :init
  (setenv "WORKON_HOME" "/Users/lix/anaconda3/envs/")
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (defconst python-version "python3")
  (defun lix/echo-python-version()
    (interactive)
    (message (shell-command-to-string "python --version")))
  (defun lix/switch-to-python2 ()
    (interactive)
    (pyvenv-workon "python2")
    (message "Python2 is running!"))
  (defun lix/switch-to-python3 ()
    (interactive)
    (pyvenv-workon "..")
    (message "Python3 is running!"))
  (defun lix/toggle-python-version()
    "Toggle Python Version."
    (interactive)
    (let ((pyv (substring (shell-command-to-string "python --version") 7 8)))
      (when (string= pyv "3")
        (switch-to-python2))
      (when (string= pyv "2")
        (switch-to-python3)))))

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

(provide 'config-data-science)
;;; config-data-science.el ends here

