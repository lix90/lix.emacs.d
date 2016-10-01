;;; init.el
;; Debugging

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq message-log-max 10000)

(defvar local-dir user-emacs-directory
  "The root dir of the Emacs configuration.")

(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name local-dir))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))

(setq load-prefer-newer t)

(load (local-file-name "config/config-package.el"))
(load (local-file-name "config/config-os.el"))
(load (local-file-name "config/config-edit"))
(load (local-file-name "config/config-keybind"))
(load (local-file-name "config/config-looking"))

;; productivity
(load (local-file-name "config/config-fm")) ;; file management
(load (local-file-name "config/config-paren"))
(load (local-file-name "config/config-ac")) ;; auto completion
;; tools
(load (local-file-name "config/config-git"))

;; languages
(load (local-file-name "config/config-ess"))
(load (local-file-name "config/config-python"))
(load (local-file-name "config/config-matlab"))
(load (local-file-name "config/config-ruby"))
(load (local-file-name "config/config-web"))
(load (local-file-name "config/config-cc"))
(load (local-file-name "config/config-sql"))
(load (local-file-name "config/config-java"))
(load (local-file-name "config/config-tex"))

;; write
(load (local-file-name "config/config-markdown"))
;; (load (local-file-name "config/config-blog"))

;;; end init.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (latex-preview-pane yaml-mode whole-line-or-region whitespace-cleanup-mode which-key web-mode web-beautify use-package undo-tree spaceline smartparens rainbow-delimiters py-autopep8 psysh php-mode nodejs-repl neotree monokai-theme matlab-mode markdown-toc magit-gitflow json-mode js2-mode js-comint hungry-delete flx fix-word expand-region exec-path-from-shell ess elpy disaster counsel company-emacs-eclim company-auctex company-anaconda color-theme-approximate clang-format better-defaults benchmark-init aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
