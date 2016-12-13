;;; init --- init file:
;;; Commentary:
;; Debugging
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

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

(load (local-file-name "config/config-core"))
(load (local-file-name "config/config-os"))
(load (local-file-name "config/config-edit"))
(load (local-file-name "config/config-appearance"))
(load (local-file-name "config/config-shell"))
(load (local-file-name "config/config-private"))

;; Productivity
(load (local-file-name "config/config-ac"))
(load (local-file-name "config/config-dired"))
(load (local-file-name "config/config-histfile"))
(load (local-file-name "config/config-navigation"))
(load (local-file-name "config/config-paren"))
(load (local-file-name "config/config-swiper"))
(load (local-file-name "config/config-whichkey"))
(load (local-file-name "config/config-evil"))

;; Tools
(load (local-file-name "config/config-tool"))
;; (load (local-file-name "config/config-flycheck"))
;; (load (local-file-name "config/config-blog"))

;; Languages
(load (local-file-name "config/config-ess"))
(load (local-file-name "config/config-python"))
(load (local-file-name "config/config-matlab"))
(load (local-file-name "config/config-tex"))
(load (local-file-name "config/config-web"))
(load (local-file-name "config/config-sql"))
(load (local-file-name "config/config-markdown"))
;; (load (local-file-name "config/config-java"))
(load (local-file-name "config/config-ruby"))
;; (load (local-file-name "config/config-cc"))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elfeed youdao-dictionary yaml-mode yafolding whole-line-or-region whitespace-cleanup-mode which-key weibo web-mode web-beautify web w3m use-package symon sr-speedbar spaceline solarized-theme smartparens smart-mode-line simple-httpd shell-pop scss-mode rvm robe rainbow-mode rainbow-delimiters py-autopep8 pug-mode psysh projectile project-explorer polymode php-eldoc phi-rectangle paradox osx-dictionary origami multiple-cursors move-text monokai-theme moe-theme mmm-mode matlab-mode markdown-toc magithub magit-gitflow lorem-ipsum latex-preview-pane json-mode js2-mode js-comint indent-guide hungry-delete hexo help-fns+ google-translate golden-ratio flycheck flx fix-word expand-region exec-path-from-shell evil-nerd-commenter evil-magit evil-leader esup ess emmet-mode elpy doremi-frm doremi-cmd disaster dired-details dash-functional dash-at-point crux counsel company-web company-statistics company-php company-emoji company-emacs-eclim company-auctex company-anaconda color-theme clang-format blog-admin bing-dict better-defaults benchmark-init auto-package-update aggressive-indent ace-jump-mode ac-php))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
