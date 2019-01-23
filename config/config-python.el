;;; package --- Summary:

;;; Commentary:

;;; Code:
(defconst my/python-env-path "/usr/local/miniconda3/envs/")

(use-package python :ensure t :defer t
  :commands (run-python)
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
              ("RET" . electric-newline-and-maybe-indent))
  ;; (remove-hook 'python-mode-hook #'python-setup-shell)
  :init
  (setenv "JUPYTER_CONSOLE_TEST" "1")
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (setenv "WORKON_HOME" my/python-env-path)
  (setq python-shell-completion-native-enable nil)
  :config
  (setq mode-name "Python"
        python-indent-offset 4
        indent-tabs-mode nil
        tab-width 4
        fill-column 80
        comment-inline-offset 2
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--pylab=osx --nosep --classic"
        ;;python-shell-interpreter-args "--pylab=osx --pdb --nosep --classic"
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out \\[[0-9]+\\]: "
        ;;python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
        ;;python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
        )
  (add-hook 'python-mode-hook (lambda ()
                                (electric-indent-mode -1)
                                (aggressive-indent-mode -1)))

  (add-hook 'inferior-python-mode-hook (lambda ()
                                         (company-mode-on)))

  ;; elpy configurations
  (use-package elpy :ensure t :defer t
    :bind (:map elpy-mode-map
                ("C-<return>" . elpy-shell-send-statemend-and-go)
                ("C-c C-f" . elpy-shell-send-defun-and-go)
                ("C-c p r" . elpy-refactor)
                ("C-c p f" . elpy-format-code)
                )
    :init
    (elpy-enable)
    (add-to-list 'company-backends #'elpy-company-backend)
    )

  ;; anaconda mode
  (use-package company-anaconda :ensure t :defer t
    :init
    (add-to-list 'company-backends #'(company-anaconda :with company-capf)))

  (use-package anaconda-mode :ensure t :defer t
    :diminish anaconda-mode
    :init
    (setq anaconda-mode-installation-directory
          (expand-file-name ".cache/anaconda-mode" user-emacs-directory)))
  )


(use-package python-environment :defer t :ensure t
  :config
  (setq python-environment-directory "/usr/local/miniconda3/envs")
  (setq python-environment-default-root-name "ds"))

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
