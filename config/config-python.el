;;; package --- Summary:

;;; Commentary:

;;; Code:

(use-package python :ensure t :defer t
  :commands (run-python)
  :mode ("\\.py\\'" . python-mode)
  ;; (remove-hook 'python-mode-hook #'python-setup-shell)
  :init
  (setq python-shell-completion-native-enable nil)
  (setenv "JUPYTER_CONSOLE_TEST" "1")
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (setenv "WORKON_HOME" "/usr/local/miniconda3/envs")
  :config
  (setq mode-name "Python"
        python-indent-offset 4
        indent-tabs-mode nil
        tab-width 4
        fill-column 80
        comment-inline-offset 2
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--pylab=osx --pdb --nosep --classic"
        python-shell-prompt-regexp ">>> "
        python-shell-prompt-output-regexp ""
        ;;python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        ;;python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        )
  ;;python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
  ;;python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
  ;;python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  (set (make-local-variable 'electric-indent-mode) nil)
  (add-hook 'python-mode-hook (lambda ()
                                (company-mode t)
                                (add-to-list 'company-backends 'elpy-company-backend)))
  )

(use-package python-environment :defer t :ensure t
  :config
  (setq python-environment-directory "/usr/local/miniconda3/envs")
  (setq python-environment-default-root-name "ds")
  )

(use-package elpy :ensure t :defer t
  :config
  (elpy-enable)
  (defun elpy-goto-definition-or-rgrep ()
    "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
      (error (elpy-rgrep-symbol
              (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
  (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep))


(use-package company-jedi :ensure t :defer t :disabled t
  :config
  (setq jedi:environment-root "ds")
  (setq jedi:environment-virtualenv (list (expand-file-name "~/usr/local/miniconda3/envs/")))
  (setq jedi:complete-on-dot t
        jedi:use-shortcuts t
        company-tooltip-align-annotations t
        company-transformers '(company-sort-by-occurrence)
        company-selection-wrap-around t)
  )


;; python拓展功能包
;;  + anaconda-mode
;;  + company-anaconda
;; (use-package company-anaconda :ensure t :defer t :disabled t
;;   :init
;;   (add-to-list 'company-backends #'company-anaconda))

;; (use-package anaconda-mode :ensure t :defer t :disabled t
;;   :diminish anaconda-mode
;;   :init
;;   (setq anaconda-mode-installation-directory
;;         (expand-file-name ".cache/anaconda-mode" user-emacs-directory)))



;; (defun python-send-line ()
;;   (interactive)
;;   (save-excursion
;;     (back-to-indentation)
;;     (python-shell-send-string
;;      (concat (buffer-substring-no-properties
;;               (point)
;;               (line-end-position))
;;              "\n"))))

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "s-<return>")
;;                            (lambda ()
;;                              (interactive)
;;                              (python-send-line)
;;                              (forward-line)))))

;; (when (version< emacs-version "24.5")
;;   ;; auto-indent on colon doesn't work well with if statement
;;   ;; should be fixed in 24.5 and above
;;   (setq electric-indent-chars (delq ?: electric-indent-chars)))

;; (add-hook 'python-mode-hook
;;           (lambda()
;;             (flycheck-mode t)
;;                                         ;(anaconda-mode t)
;;                                         ;(anaconda-eldoc-mode t)
;;                                         ;(electric-indent-mode t)
;;             ;; key-bindings
;;             (local-set-key (kbd "C-j") #'newline-and-indent)
;;             (local-set-key (kbd "C-c C-r") #'python-shell-send-region)
;;             (local-set-key (kbd "C-c C-d") #'python-shell-send-defun)
;;             (local-set-key (kbd "s-<return>")
;;                            (lambda ()
;;                              (interactive)
;;                              (python-send-line)
;;                              (forward-line)))))
;; set local company-mode
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '((company-anaconda company-dabbrev-code)
;;                    company-dabbrev))
;;             (smartparens-mode +1)))

(use-package pyvenv :ensure t :defer t :disabled t)


(provide 'config-python)
;;; config-python.el ends here
