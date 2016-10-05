;; mac specific settings
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (set-fontset-font "fontset-default" 'gb18030' ("STHeiti" . "unicode-bmp"))
  (set-frame-font "Monaco 12"))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; configure shell
(require 'eshell)
(setq eshell-directory-name (local-file-name "cache/eshell"))

(setq shell-file-name "/bin/bash")
(global-set-key (kbd "C-c z") 'shell)
(global-set-key (kbd "<f10>") 'rename-buffer)

;; set coding system
(setenv "LANG" "zh_CN.UTF-8")


(provide 'config-os)

;;; config-os.el ends here
