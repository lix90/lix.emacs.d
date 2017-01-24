(setq org-agenda-files (list "~/org/work.org"
                             "~/org/learning.org"
                             "~/org/notes.org")
      org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(provide 'config-org)
;;; config-org file ends here
