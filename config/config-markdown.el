(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode))
  :init
  (progn
    ;; markdown hooks
    (add-hook 'markdown-mode-hook
              '(lambda()
                 (turn-on-auto-fill) (linum-mode) (centered-cursor-mode) (set-fill-column 78) (flyspell-mode 1) (pandoc-mode) (hl-todo-mode) (git-gutter+-mode t)))
    (setq markdown-command "pandoc"
          markdown-enable-math t
          ;; markdown-footnote-location "end"
          markdown-nested-imenu-heading-index t
          ;;markdown-open-command "/Users/Roambot/bin/scripts/mark.sh"
          )
    ;; (global-git-gutter+-mode t)
    ;; (add-hook 'markdown-mode-hook #'centered-cursor-mode)
    ;; (add-hook 'markdown-mode-hook #'pandoc-mode)
    ;; (add-hook 'markdown-mode-hook #'hl-todo-mode)
    ;; (add-hook 'markdown-mode-hook #'flyspell-mode)
    ;; add keybindings to hook
    )
  :config
  (progn;; Header navigation in normal state movements
    (general-define-key :states '(normal) :keymaps 'markdown-mode-map
                        "TAB" 'markdown-cycle
                        "gj"  'outline-forward-same-level
                        "gk"  'outline-backward-same-level
                        "gh"  'outline-up-heading
                        ;; next visible heading is not exactly what we want but close enough
                        "gl"  'outline-next-visible-heading)
    ;; "<return>" 'markdown-jump
    ;; Promotion, Demotion
    (general-define-key :states '(normal insert emacs) :keymaps 'markdown-mode-map
                        "M-h" 'markdown-promote
                        "M-j" 'markdown-move-down
                        "M-k" 'markdown-move-up
                        "M-l" 'markdown-demote
                        ;; fix wrong emacs keybindings
                        "C-c C-j" 'markdown-jump
                        "C-c C-l" 'markdown-insert-list-item)
    (add-hook 'markdown-mode-hook #'my-markdown-config)))

(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode))

(defun distraction-free ()
  "distraction free writing"
  (interactive)
  (git-gutter+-mode 0)
  (linum-mode 0)
  (writeroom-mode))

;; -----------------------------------
;; markdown
;; -----------------------------------
;; (use-package markdown-mode
;;   :ensure t
;;   :commands markdown-mode
;;   :mode (("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode)
;;          ("\\.text\\'" . markdown-mode))
;;   :config (progn
;;             (setq-local line-spacing 0.1)
;;             (add-hook 'markdown-mode-hook 'visual-line-mode)
;;             (add-hook 'markdown-mode-hook 'company-mode)
;;             (add-hook 'markdown-mode-hook 'smartparens-mode)
;;             (use-package markdown-toc
;;               :ensure t)
;;             ))

(provide 'config-markdown)

;;; config-markdown.el ends here
