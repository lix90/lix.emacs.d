;;; init.el
;; Debugging
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

(load (local-file-name "config/config-keybind"))
(load (local-file-name "config/config-looking"))
(load (local-file-name "config/config-edit"))
(load (local-file-name "config/config-tool"))
(load (local-file-name "config/config-lang"))

;;; end init.el
