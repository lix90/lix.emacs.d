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
(load (local-file-name "config/config-appearance"))
(load (local-file-name "config/config-shell"))
(load (local-file-name "config/config-private"))
(load (local-file-name "config/config-navigation"))
(load (local-file-name "config/config-prog"))
(load (local-file-name "config/config-histfile"))
(load (local-file-name "config/config-paren"))
(load (local-file-name "config/config-swiper"))
(load (local-file-name "config/config-whichkey"))
(load (local-file-name "config/config-evil"))
(load (local-file-name "config/config-keybindings"))

;; Tools
(load (local-file-name "config/config-tool"))
(load (local-file-name "config/config-org"))
(load (local-file-name "config/config-git"))
;; (load (local-file-name "config/config-flycheck"))
;; (load (local-file-name "config/config-blog"))

;; Languages
(load (local-file-name "config/config-ess"))
(load (local-file-name "config/config-python"))
(load (local-file-name "config/config-matlab"))
(load (local-file-name "config/config-tex"))
(load (local-file-name "config/config-web"))
(load (local-file-name "config/config-markdown"))
;; (load (local-file-name "config/config-sql"))
;; (load (local-file-name "config/config-java"))
;; (load (local-file-name "config/config-ruby"))
;; (load (local-file-name "config/config-cc"))

;;; init.el ends here
