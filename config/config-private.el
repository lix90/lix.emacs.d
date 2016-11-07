;;; private configuration el file

;; open folders
(defun lix/open-emacs-config ()
  "Open emacs config directory."
  (interactive)
  (find-file "~/.emacs.d/config/"))
(global-set-key (kbd "C-c u o c") 'lix/open-emacs-config)

(defun lix/open-hexo-root ()
  "Open hexo root directory."
  (interactive)
  (find-file "~/github/hexo-blog/"))
(global-set-key (kbd "C-c u o h") 'lix/open-hexo-root)

(defun lix/open-hexo-source-draft ()
  "Open hexo draft directory."
  (interactive)
  (find-file "~/github/hexo-blog/source/_drafts/"))
(global-set-key (kbd "C-c u o d") 'lix/open-hexo-source-draft)

(defun lix/open-hexo-source-post ()
  "Open hexo draft directory."
  (interactive)
  (find-file "~/github/hexo-blog/source/_posts/"))
(global-set-key (kbd "C-c u o p") 'lix/open-hexo-source-post)

(defun lix/open-jirengu-me ()
  "Open jirengu repository."
  (interactive)
  (find-file "~/jirengu/jrg-renwu9/homework/李想/"))
(global-set-key (kbd "C-c u o j") 'lix/open-jirengu-me)

(defun lix/file-hexo-source-about ()
  "Open hexo about."
  (interactive)
  (find-file "~/github/hexo-blog/source/about/index.md"))
(global-set-key (kbd "C-c u o a") 'lix/file-hexo-source-about)

;; create new post
(defun lix/open-hexo-create-post ()
  "Create and open post file."
  (interactive)
  (setq fn (read-string "Enter file name:"))
  ;; (shell-command
  ;;  (format "cd ~/github/hexo-blog/ && hexo new post \"%s\"" fn))
  (setq time (format-time-string "%Y-%m-%d" (current-time)))
  (find-file
   (format "~/github/hexo-blog/source/_posts/%s-%s.md" time fn))
  (erase-buffer)
  (yas-insert-snippet)
  )

(defun lix/file-note-temp ()
  "Open note temp file."
  (interactive)
  (find-file "~/github/temp.md"))
(global-set-key (kbd "C-c u o n") 'lix/file-note-temp)


;; kill all buffers
(defun lix/util-close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-c u k") 'lix/util-close-all-buffers)


(global-set-key (kbd "C-h") nil)
(bind-key "<f1>" #'help-command)
(bind-key "C-h" #'delete-backward-char)
(bind-key "M-h" #'backward-kill-word)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

(defun lix/open-file-init ()
  "Open init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c u o i") 'lix/open-file-init)

(global-set-key [f9] 'set-mark-command)
(global-set-key (kbd "C-c u e b") 'eval-buffer)
(global-set-key (kbd "C-c u e r") 'eval-region)
(global-set-key (kbd "C-c u j") 'join-line)
;; resize window
(global-set-key (kbd "C-S-l") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-h") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-p") 'shrink-window)
(global-set-key (kbd "C-S-n") 'enlarge-window)
;; insert data
(defun lix/util-insert-date ()
  (interactive)
  ;; (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
  (insert (format-time-string "%Y-%m-%d" (current-time)))
  )
(global-set-key (kbd "C-c u u d") 'lix/util-insert-date)
;; coding-system
(setq buffer-file-coding-system 'utf-8-unix)

;;--------------------------------------------------------------------
;; set other window
;; next window
;; (defun other-window-backward ( )
;;   "select the previous window."
;;   (interactive)
;;   (other-window -1))
;; (global-set-key "\C-x\C-n" 'other-window)
;; (global-set-key "\C-x\C-p" 'other-window-backward)

;; window split
(defun lix/util-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c u |") 'lix/util-toggle-window-split)

;; easy comment
(defun lix/util-comment-or-uncomment-region (beg end &optional arg)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (line-beginning-position)
                       (line-beginning-position 2))))
  (comment-or-uncomment-region beg end arg))
(global-set-key [remap comment-or-uncomment-region] 'lix/util-comment-or-uncomment-region)
(global-set-key [f6] 'comment-or-uncomment-region)

;;--------------------------- for R ------------------------------
(defun lix/open-r-package ()
  (interactive)
  "Open r site-package."
  (find-file "/usr/local/Cellar/r/3.3.1_3/R.framework/Resources/site-library/"))
(global-set-key (kbd "C-c u o R") 'lix/open-r-package)




(provide 'config-private)
;; config-private.el ends here
