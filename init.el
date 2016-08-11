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

(load (local-file-name "core/core-packages"))
;; (load (local-file-name "config/config-os"))
(load (local-file-name "config/config-keybind"))
(load (local-file-name "config/config-looking"))
(load (local-file-name "config/config-edit"))
(load (local-file-name "config/config-tool"))
(load (local-file-name "config/config-lang"))

;;; end init.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck whole-line-or-region whitespace-cleanup-mode which-key use-package undo-tree swiper sublimity spaceline smex smartparens rainbow-delimiters pkg-info paredit neotree multiple-cursors monokai-theme matlab-mode markdown-toc magit-gitflow hungry-delete hlinum helm-pydoc helm fix-word expand-region exec-path-from-shell evil-nerd-commenter esup ess elpy ein company-emoji company-anaconda color-theme-approximate better-defaults aggressive-indent ac-anaconda))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
