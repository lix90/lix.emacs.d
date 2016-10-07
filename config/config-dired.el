;;;-----------------------------------
;;; Dired setup
;;;-----------------------------------
(require 'dired)
(require 'dash)
(use-package dired-details
  :ensure t
  :init
  (setq-default dired-details-hidden-string "--- "))

(setq dired-dwim-target t)

;; Reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
           (revert-buffer))))

(provide 'config-dired)
;;; config-dired.el ends here
