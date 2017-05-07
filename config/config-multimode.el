;; Rmd in emacs
;; reference: http://futurismo.biz/archives/2982
(use-package polymode :ensure t
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

(provide 'config-multimode)
