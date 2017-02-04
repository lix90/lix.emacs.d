;;; init --- init file:
;;; Commentary:

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 10000)

(require 'cl)

(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name user-emacs-directory))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))

(setq load-prefer-newer t)

(defun load-config(suffix)
  (setq fname (concat "config/config-" suffix))
  (load (local-file-name fname)))

(load-config "core")
(load-config "appearance")
(load-config "private")
(load-config "navigation")
(load-config "evil")
(load-config "keybindings")
(load-config "shell")
(load-config "programming")
(load-config "version-control")
(load-config "data-science")
(load-config "web-development")
(load-config "writing")
(load-config "languages")
(load-config "utilities")

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
;;; init.el ends here
