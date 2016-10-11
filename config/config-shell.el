;; configure shell
(use-package shell-pop
  :ensure t
  :init
  (progn
    (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
    (setq shell-pop-term-shell "/bin/bash")
    (setq shell-pop-universal-key "C-c z")
    (setq shell-pop-window-size 30)
    (setq shell-pop-full-span t)
    (setq shell-pop-window-position "bottom")))

;; (setq shell-file-name "/bin/bash")
;; (global-set-key (kbd "C-c z") 'shell)
(global-set-key (kbd "<f10>") 'rename-buffer)

;; set coding system
(setenv "LANG" "zh_CN.UTF-8")

(provide 'config-shell)
;;; config-shell.el ends here
