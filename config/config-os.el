
;; mac specific settings
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (set-fontset-font "fontset-default" 'gb18030' ("STHeiti" . "unicode-bmp"))
  (set-frame-font "Source Code Pro 13"))


(provide 'config-os)

;;; config-os.el ends here
