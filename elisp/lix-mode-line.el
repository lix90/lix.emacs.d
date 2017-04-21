(require 'all-the-icons)

(defvar active (eq (frame-selected-window) (selected-window)))

(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun spaceline---github-vc ()
  "Function to return the Spaceline formatted GIT Version Control text."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (all-the-icons-alltheicon "git") 'face '(:height 0.9 :inherit) 'display '(raise 0.1))
     (propertize " ")
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:family ,(all-the-icons-octicon-family) :height 0.8 :inherit)
                 'display '(raise 0.1))
     (propertize (format " %s" branch) 'face `(:height 0.8 :inherit) 'display '(raise 0.1)))))

(defun spaceline---svn-vc ()
  "Function to return the Spaceline formatted SVN Version Control text."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
     (propertize (format " %s" revision) 'face `(:height 0.8)))))

;;; mode line function
(defun lix/get-buf-state()
  "Obtain buffer state."
  (let* ((config-alist
          '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
            ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
            ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))

    (propertize (format "%s" (apply (cadr result) (cddr result))) 'face `(:family ,(funcall (car result)) :inherit ))))


(defun lix/get-win-number()
  (when (fboundp 'winum-mode)
    (propertize (format "%c" (+ 9311 (winum-get-number)))
                'face '(:height 1 :inherit 'all-the-icons-yellow)
                'display '(raise 0.1))))

;; projectile
(defun lix/get-projectile-proj()
  (concat
   (if (and (fboundp 'projectile-project-name)
            (projectile-project-name))
       (propertize (format "%s" (concat (projectile-project-name)))
                   'face '(:height 0.8 :weight bold)
                   'display '(raise 0.1)
                   'help-echo "Switch Project"
                   'mouse-face '(:box 1)
                   'local-map (make-mode-line-mouse-map
                               'mouse-1 (lambda () (interactive) (projectile-switch-project))))
     (propertize "×" 'face '(:height 0.8 :inherit)))))

;; mode icons
(defun lix/get-mode-icon()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (propertize icon
                  'help-echo (format "Major-mode: `%s`" major-mode)
                  'display '(raise -0.1)
                  'face `(:height 1.1 :family ,(all-the-icons-icon-family-for-buffer) :inherit 'all-the-icons-yellow)))))

;; buffer id
(defun lix/get-buffer-id()
  (if (fboundp 'projectile-project-root)
      (let* ((buf (or (buffer-file-name) (buffer-name)))
             (proj (ignore-errors (projectile-project-root)))
             (fname (if (buffer-file-name)
                        (file-name-nondirectory buf)
                      (format-mode-line "%b")))
             (dirname (if (buffer-file-name)
                          (cadr (split-string (file-name-directory buf) proj))
                        "")))
        (concat
         (propertize dirname
                     'face '(:height 0.8 :inherit)
                     'display '(raise 0.1))
         (propertize (format "%s" fname)
                     'face '(:height 0.8 :weight bold :inherit 'all-the-icons-yellow)
                     'display '(raise 0.1)
                     'help-echo (format "Major-mode: `%s`" major-mode))))
    (propertize (format-mode-line "%b ")
                'face '(:weight bold :height 0.8 :inherit 'all-the-icons-yellow)
                'display '(raise 0.1))))

;; current process
(defun lix/get-current-process
    (let ((icon (all-the-icons-icon-for-buffer)))
      (concat
       (when (or (symbolp icon) mode-line-process)
         (propertize (format-mode-line "%m") 'face `(:height 0.8 :inherit) 'display '(raise 0.2)))
       (when mode-line-process
         (propertize (format-mode-line mode-line-process) 'face '(:height 0.7 :inherit) 'display '(raise 0.2))))))

;; current point
(defun lix/get-current-point--line()
  ;;(propertize (format-mode-line "%4l:%3c") 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
  (propertize (format-mode-line "%4l:")
              'face '(:height 0.8 :inherit)
              'display '(raise 0.1)))

(defun lix/get-current-point--col()
  (propertize (format-mode-line "%3c")
              'face
              (if (>= (current-column) 80)
                  '(:height 0.8 :weight bold :inherit 'all-the-icons-yellow)
                '(:height 0.8 :inherit))
              'display '(raise 0.1)))

;; current marked region
(defun lix/get-marked-region()
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize (format "%s " (all-the-icons-octicon "pencil") words chars)
                   'face `(:family ,(all-the-icons-octicon-family) :inherit) 'display '(raise 0.1))
       (propertize (format "(%s, %s)" words chars)
                   'face `(:height 0.9 :inherit))))))


(defun lix/get-git-stats()
  (when (and active
             (fboundp 'git-gutter:statistic)
             (or (> (car (git-gutter:statistic)) 0)
                 (> (cdr (git-gutter:statistic)) 0)))
    (pcase-let ((`(,added . ,deleted) (git-gutter:statistic)))
      (concat
       (when (> added 0)
         (concat
          (propertize
           (format "%s" (all-the-icons-octicon "diff-added" :v-adjust 0.1 :height 0.8))
           'face `(:foreground ,(face-foreground 'success) :family ,(all-the-icons-octicon-family)))
          (propertize " " 'face `(:height 0.4))
          (propertize (format "%s" added) 'face `(:foreground ,(face-foreground 'success)))))
       (when (and (> deleted 0) (> added 0)) " ")
       (when (> deleted 0)
         (concat
          (propertize
           (format "%s" (all-the-icons-octicon "diff-removed" :v-adjust 0.1 :height 0.8))
           'face `(:foreground ,(face-foreground 'spaceline-flycheck-error) :family ,(all-the-icons-octicon-family)))
          (propertize " " 'face `(:height 0.4))
          (propertize (format "%s" deleted) 'face `(:foreground ,(face-foreground 'spaceline-flycheck-error)))))))))

(defun lix/get-vc-icon()
  (when (and active vc-mode)
    (cond ((string-match "Git[:-]" vc-mode) (spaceline---github-vc))
          ((string-match "SVN-" vc-mode) (spaceline---svn-vc))
          (t (propertize (format "%s" vc-mode))))))


(defun lix/get-flychecker()
  (when (and active
             (boundp 'flycheck-last-status-change))
    (let* ((text
            (pcase flycheck-last-status-change
              (`finished (if flycheck-current-errors
                             (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                            (+ (or .warning 0) (or .error 0)))))
                               (format "✖ %s Issue%s" count (if (eq 1 count) "" "s")))
                           "✔ No Issues"))
              (`running     "⟲ Running")
              (`no-checker  "⚠ No Checker")
              (`not-checked "✖ Disabled")
              (`errored     "⚠ Error")
              (`interrupted "⛔ Interrupted")
              (`suspicious  "")))
           (f (cond
               ((string-match "⚠" text) `(:height 0.9 :foreground ,(face-attribute 'spaceline-flycheck-warning :foreground)))
               ((string-match "✖ [0-9]" text) `(:height 0.9 :foreground ,(face-attribute 'spaceline-flycheck-error :foreground)))
               ((string-match "✖ Disabled" text) `(:height 0.9 :foreground ,(face-attribute 'font-lock-comment-face :foreground)))
               (t '(:height 0.9 :inherit)))))
      (propertize (format "%s" text)
                  'face f
                  'help-echo "Show Flycheck Errors"
                  'display '(raise 0.1)
                  'mouse-face '(:box 1)
                  'local-map (make-mode-line-mouse-map 'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))))

(defun lix/get-flycheck-status()
  (when (and active
             (boundp 'flycheck-last-status-change)
             flycheck-current-errors)
    (let-alist flycheck-current-errors
      (when .info
        (let ((text (format "%s%s" (all-the-icons-faicon "info") .info)))
          (propertize (format "%s" text)
                      'face f
                      'help-echo "Show Flycheck Errors"
                      'display '(raise 0.2)
                      'mouse-face '(:box 1)
                      'local-map (make-mode-line-mouse-map 'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))))))

(defun lix/get-buf-position()
  (propertize (format-mode-line "%p")))

(defun lix/get-buf-size()
  (propertize (format-mode-line "%I")
              'face `(:height 0.9 :inherit) 'display '(raise 0.1)))

(defun lix/get-time()
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :v-adjust 0.0)))
    (concat
     (propertize (format "%s" icon)
                 'face `(:height 1 :family ,(all-the-icons-wicon-family) :inherit)
                 'display '(raise 0))
     " "
     (propertize (format-time-string "%H:%M") 'face `(:height 0.9 :inherit) 'display '(raise 0.1)))))

(setq-default mode-line-format
              (list
               " "
               '(:eval (lix/get-win-number))
               " "
               '(:eval (lix/get-buf-state))
               ;; '(:eval (lix/get-buf-size))
               " "
               '(:eval (lix/get-current-point--line))
               '(:eval (lix/get-current-point--col))
               "  "
               '(:eval (lix/get-mode-icon))
               " "
               '(:eval (lix/get-buffer-id))
               " "
               '(:eval (lix/get-projectile-proj))

               ;;"/"
               ;;'(:eval (lix/get-buf-position))

               " "
               '(:eval (lix/get-vc-icon))
               " "
               '(:eval (lix/get-flychecker))
               " "
               '(:eval (lix/get-flycheck-status))
               ;; blank
               (propertize (mode-line-fill 'mode-line 20) 'face '(:inherit))
               ;; time
               '(:eval (lix/get-time))
               ;;" "
               ;;'(:eval (lix/get-marked-region))

               ))

(defvar lix/mode-line--colors-alist
  '((default-theme ((active . ((background . "#555") (foreground . "#fff")))
                    (inactive . ((background . "#333") (foreground . "#555")))))
    (atom-one-dark ((active . ((background . "#655370") (foreground . "#fff")))
                    (inactive . ((background . "#333") (foreground . "#555")))))
    (spacemacs-dark ((active . ((background . "#5d4d7a") (foreground . "#fff")))
                     (inactive . ((background . "#333") (foreground . "#555")))))
    (doom-molokai ((active . ((background . "#727280") (foreground . "#fff")))
                   (inactive . ((background . "#333") (foreground . "#555")))))
    (doom-one ((active . ((background . "#1f5582") (foreground . "#fff")))
               (inactive . ((background . "#333") (foreground . "#555")))))
    (solarized-dark ((active . ((background . "#268bd2") (foreground . "#fff")))
                     (inactive . ((background . "#333") (foreground . "#555")))))
    (solarized-light ((active . ((background . "#9ea0e5") (foreground . "#fff")))
                      (inactive . ((background . "#073642") (foreground . "#555")))))))

(defun lix/set-face (face alist)
  (let-alist alist
    (set-face-attribute face nil
                        :foreground .foreground
                        :background .background
                        :underline nil))
  (if (not show-paren-mode) (show-paren-mode t)
    (set-face-attribute 'show-paren-match-face nil
                        :weight 'bold
                        :foreground "red"
                        :background nil))
  (if (not show-smartparens-global-mode) (show-smartparens-global-mode t)
    (set-face-attribute 'sp-show-pair-match-face nil
                        :weight 'bold
                        :foreground "red"
                        :background nil)))
;;;###autoload
(defun lix/update-faces(&rest args)
  (interactive)
  (let ((theme-alist (cadr (assoc (car custom-enabled-themes) lix/mode-line--colors-alist)))
        (theme-default (cadr (assoc 'default-theme lix/mode-line--colors-alist))))
    (when theme-alist
      (let-alist theme-alist
        (lix/set-face 'mode-line .active)
        (lix/set-face 'mode-line-inactive .inactive)))
    (when (not theme-alist)
      (let-alist theme-default
        (lix/set-face 'mode-line .active)
        (lix/set-face 'mode-line-inactive .inactive)))))

  ;; ;; (when theme
  ;; ;;   (load-theme theme t))
  ;; (set-face-attribute 'mode-line nil
  ;;                     :background "#555"
  ;;                     :foreground "#eee"
  ;;                     :box nil
  ;;                     :underline nil)
  ;; (set-face-attribute 'mode-line-inactive nil
  ;;                     :background "#181e26"
  ;;                     :foreground "#333"
  ;;                     :box nil
  ;;                     :underline nil))

(provide 'lix-mode-line)
