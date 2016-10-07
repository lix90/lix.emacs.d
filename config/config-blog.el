;;; package --- summary
;; CodeFalling/blog-admin

;;; Code:
(use-package blog-admin
  :ensure t
  :init
  (progn
    (setq blog-admin-backend-path "~/github/hexo-blog/")
    (setq blog-admin-backend-type 'hexo)
    (setq blog-admin-backend-new-post-in-drafts t) ;; create new post in drafts by default
    (setq blog-admin-backend-new-post-with-same-name-dir t) ;; create same-name directory with new post
    (setq blog-admin-backend-hexo-config-file "_config.yml") ;; default assumes _config.yml)
    )
  )

(use-package hexo
  :ensure t)
(require 'hexo)

(defun hexo-my-blog ()
  (interative)
  (hexo "~/github/hexo-blog/"))


(provide 'config-blog)
