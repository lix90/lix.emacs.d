;; configure shell
;; (use-package shell-pop
;;   :ensure t
;;   :init
;;   (progn
;;     (setq shell-pop-shell-type (quote ("shell" "*shell*" (lambda nil (shell)))))
;;     (setq shell-pop-term-shell "/bin/bash")
;;     (setq shell-pop-universal-key "C-c u z")
;;     (setq shell-pop-window-size 30)
;;     (setq shell-pop-full-span t)
;;     (setq shell-pop-window-position "bottom")))

(setq shell-file-name "/bin/bash")
(setq explicit-bash-args '("--login" "--init-file" "$HOME/.bash_profile" "-i"))
(defalias 'sh 'shell)
(global-set-key (kbd "C-c u r") 'rename-buffer)

;; set coding system
(setenv "LANG" "zh_CN.UTF-8")

(provide 'config-shell)
;;; config-shell.el ends here
