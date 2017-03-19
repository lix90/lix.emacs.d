;;; useR --- ess
;;; Commentary:
;;; Code:

(use-package auto-complete :ensure t :defer t  
  :config
  (progn
    (setq 
     ;; ac-auto-show-menu 1
     ;; ac-candidate-limit nil
     ;; ac-delay 0.1
     ;; ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face))
     ac-ignore-case 'smart
     ac-menu-height 10
     ac-quick-help-delay 1
     ac-quick-help-prefer-pos-tip t
     ac-use-quick-help t
     )
    (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
    (define-key ac-completing-map "\M-n" nil) ;; was ac-next
    (define-key ac-completing-map "\M-p" nil) ;; was ac-previous
    (define-key ac-completing-map "\M-," 'ac-next)
    (define-key ac-completing-map "\M-k" 'ac-previous)
    (define-key ac-completing-map [tab] 'ac-complete)
    (define-key ac-completing-map [return] nil)))

(use-package ess-site :ensure ess :defer t
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
    (use-package ess-smart-underscore :ensure t :defer t)
    ;; (use-package ess-R-data-view :ensure t :defer t
    ;;   :commands (ess-R-dv-ctable ess-R-dv-pprint))
    ;; (use-package ess-R-object-popup :ensure t :defer t
    ;;   :commands (ess-R-object-popup))
    ;;(el-get-bundle Lompik/company-ess) 
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
          ess-use-company nil
          ess-use-auto-complete t
          ess-eldoc-show-on-symbol t
          eldoc-echo-area-use-multiline-p t
          )

    ;;(define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input) 
    ;;(define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)
    ;;(define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
    ;;(define-key ess-mode-map (kbd "C-c v t") 'ess-R-dv-ctable)
    ;;(define-key ess-mode-map (kbd "C-c v p") 'ess-R-dv-pprint)
    ;;(define-key ess-mode-map (kbd "C-c v o") 'ess-R-object-popup)
    
    (add-hook 'ess-mode-hook 'smartparens-mode)
    (add-hook 'ess-mode-hook 'yas-minor-mode)
    (add-hook 'inferior-ess-mode-hook 'smartparens-mode)

    (defun ess-browse-vignette ()
      (interactive)
      (setq R-cmd (format "browseVignettes(\"%s\")"
                          (read-string "Enter package name:")))
      (ess-execute R-cmd 'buffer nil nil))
    (define-key inferior-ess-mode-map (kbd "C-c C-d C-v") 'ess-browse-vignette)
    
    (use-package key-combo :ensure t :defer t
      :init 
      (add-hook 'ess-mode-hook
                '(lambda()
                   (key-combo-mode t)))
      (add-hook 'inferior-ess-mode-hook
                '(lambda()
                   (key-combo-mode t)))
      (defvar key-combo-ess-default
        '((">"  . (" > " " %>% " " %>>%"))
          ("$"  . ("$" " %$% "))
          ("<>" . " %<>% ")
          ("*"  . ("*" " * " "%*%"))
          ("%" . ("%" " %in% " "%%"))
          ("^"  . ("^" " ^ "))
          ("/"  . ("/" " / "))
          ("=" . ("=" " = " " == "))
          ("!" . ("!" " != "))
          ("," . ("," ", "))
          ("~" . ("~" " ~ "))
          (":" . (":" "::" ":::"))
          (":="  . " := ") ; data.table
          ("->"  . " -> ")))
      (key-combo-define-hook
       '(ess-mode-hook inferior-ess-mode-hook)
       'ess-key-combo-load-default key-combo-ess-default))))

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
  (define-key polymode-mode-map "\M-ns" 'ess-rmarkdown)
  )


;;(define-key ess-mode-map (kbd "C-c C-d C-v") 'ess-browse-vignette)

;;; Python configuration
(use-package python :ensure t :commands (run-python)
  :mode ("\\.py\\'" . python-mode)
  :init
  (progn
	;; (remove-hook 'python-mode-hook #'python-setup-shell)
    (defun python-send-line ()
      (interactive)
      (save-excursion 
        (back-to-indentation)
        (python-shell-send-string
         (concat (buffer-substring-no-properties
                  (point)
                  (line-end-position)) 
                 "\n"))))
    
	(defun python-default ()
	  (setq mode-name "Python"
			tab-width 4
			fill-column 80
			python-shell-interpreter "ipython"
			python-shell-interpreter-args (if is-mac
                                              "--pylab=osx --matplotlib=osx --colors=Linux")
            ;; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
            ;; python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
            ;; python-shell-completion-setup-code
            ;; "from IPython.core.completerlib import module_completion"
            ;; python-shell-completion-module-string-code
            ;; "';'.join(module_completion('''%s'''))\n"
            ;; python-shell-completion-string-code
            ;; "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
            )
	  (when (version< emacs-version "24.5")
		;; auto-indent on colon doesn't work well with if statement
		;; should be fixed in 24.5 and above
		(setq electric-indent-chars (delq ?: electric-indent-chars)))
	  (setq-local comment-inline-offset 2)
      (flycheck-mode +1)
      (local-set-key (kbd "C-j") 'newline-and-indent)
	  (local-set-key (kbd "C-c C-r") #'python-shell-send-region)
      (local-set-key (kbd "C-c C-d") #'python-shell-send-defun)
      (local-set-key (kbd "s-<return>") (lambda ()
                                          (interactive)
                                          (python-send-line)
                                          (next-line))))

    (defun python-eldoc-settings ()
      ;; (eldoc-mode t)
      (anaconda-eldoc-mode t))
    
    (add-hook 'python-mode-hook #'python-default)
    (add-hook 'python-mode-hook #'python-eldoc-settings)  
    (add-hook 'python-mode-hook #'electric-indent-mode))
  :config
  (progn 
    (use-package anaconda-mode :ensure t :defer t
      :init
      (progn
        (add-hook 'python-mode-hook 'anaconda-mode)))

    (use-package company-anaconda :ensure t
      :init (add-to-list 'company-backends 'company-anaconda))

    ;; (use-package elpy :ensure t
    ;;   :init (add-to-list 'company-backends 'elpy-company-backend))

    (add-hook 'smartparens-mode 'inferior-python-mode-hook)
    (add-hook 'company-mode 'inferior-python-mode-hook)
    (add-hook 'inferior-python-mode-hook (lambda ()
                                           (setq-local company-minimum-prefix-length 0)
                                           (setq-local company-idle-delay 0.5)))		 
    (defun inferior-python-setup-hook ()
      (setq indent-tabs-mode t))
    (add-hook 'inferior-python-mode-hook #'inferior-python-setup-hook)))

(add-hook 'after-init-hook
          (lambda ()
            (defun switch-to-python2 ()
              (interactive)
              (pyvenv-workon "python2"))
            (defun switch-to-python3 ()
              (interactive)
              (pyvenv-workon ".."))
            (cond ((string-equal python-version "python2")
                   (switch-to-python2))
                  (t (switch-to-python3)))))

;; (use-package py-autopep8
;;   :ensure t
;;   :config
;;   (setq py-autopep8-options '("--max-line-length=100")))
;; (defalias 'workon 'pyvenv-workon)
;; (defalias 'runpy 'run-python)

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

;; (use-package ob-ipython
;;   :ensure t
;;   :init
;;   (setq org-babel-python-command "python2")
;;  ;;; --- goto: https://github.com/gregsexton/ob-ipython
;;   (setq org-confirm-babel-evaluate nil) ; don't prompt me to confirm
;;                                         ; everytime I want to evaluate a block
;;   ;;; display/update images in the buffer after I evaluate
;;   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
;;   ;;; ----------------------------------------
;;   )

(provide 'config-data-science)
;;; config-data-science.el ends here
