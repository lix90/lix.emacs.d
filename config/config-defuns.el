;;; Useful functions

;;; private configuration el file
;; alias
(defalias 'close-all 'lix--util-close-all-buffers)

(defun delete-window-below ()
  "Delete window below. (require 'windmove)"
  (interactive)
  (windmove-down)
  (delete-window))

(defun delete-window-above ()
  "Delete window above. (require 'windmove)"
  (interactive)
  (windmove-up)
  (delete-window))

(defun delete-window-left ()
  (interactive)
  (windmove-left)
  (delete-window))

(defun delete-window-right ()
  (interactive)
  (windmove-right)
  (delete-window))


;; Describe function/variable etc. in popup
(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point)))
    (cond
     ((fboundp thing) (describe-in-popup 'describe-function))
     ((boundp thing) (describe-in-popup 'describe-variable)))))

(defun describe-in-popup (fn)
  (let* ((thing (symbol-at-point))
         (description (save-window-excursion
                        (funcall fn thing)
                        (switch-to-buffer "*Help*")
                        (buffer-string))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))


(defun lix/dos-2-unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

;; open folders
(defun lix/goto-config ()
  "Open emacs config directory."
  (interactive)
  (find-file "~/.emacs.d/config/"))

(defun lix/open-init.el ()
  "Open init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun lix/open-custom.el()
  (interactive)
  (find-file "~/.emacs.d/custom.el"))

(defun lix/goto-hexo-root ()
  "Open hexo root directory."
  (interactive)
  (find-file "~/github/hexo-blog/"))


(defun lix/goto-hexo-source-draft ()
  "Open hexo draft directory."
  (interactive)
  (find-file "~/github/hexo-blog/source/_drafts/"))

(defun lix/open-hexo-source-post ()
  "Open hexo draft directory."
  (interactive)
  (find-file "~/github/hexo-blog/source/_posts/"))


(defun lix--open-jirengu-me ()
  "Open jirengu repository."
  (interactive)
  (find-file "~/jirengu/jrg-renwu9/homework/李想/"))

(defun lix--file-hexo-source-about ()
  "Open hexo about."
  (interactive)
  (find-file "~/github/hexo-blog/source/about/index.md"))

;; create new post
(defun lix--open-hexo-create-post ()
  "Create and open post file."
  (interactive)
  (setq fn (read-string "Enter file name:"))
  ;; (shell-command
  ;;  (format "cd ~/github/hexo-blog/ && hexo new post \"%s\"" fn))
  (setq time (format-time-string "%Y-%m-%d" (current-time)))
  (find-file
   (format "~/github/hexo-blog/source/_posts/%s-%s.md" time fn))
  (yas-insert-snippet)
  )

(defun lix--open-hexo-create-draft ()
  "Create and open draft file."
  (interactive)
  (setq fn (read-string "Enter file name:"))
  (setq time (format-time-string "%Y-%m-%d" (current-time)))
  (find-file
   (format "~/github/hexo-blog/source/_drafts/%s-%s.md" time fn))
  (yas-insert-snippet)
  )

(defun lix--file-note-temp ()
  "Open note temp file."
  (interactive)
  (find-file "~/github/temp.md"))

;; kill all buffers
(defun lix--util-close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; insert data
(defun lix--insert-date ()
  (interactive)
  ;; (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
  (insert (format-time-string "%Y-%m-%d" (current-time)))
  )


;; easy comment
(defun lix--comment-or-uncomment-region (beg end &optional arg)
  (interactive (if (use-region-p)
				   (list (region-beginning) (region-end) nil)
				 (list (line-beginning-position)
					   (line-beginning-position 2))))
  (comment-or-uncomment-region beg end arg))
(global-set-key [remap comment-or-uncomment-region] 'lix--comment-or-uncomment-region)
(global-set-key [f6] 'comment-or-uncomment-region)

;;--------------------------- for R ------------------------------
(defun lix--open-r-package ()
  (interactive)
  "Open r site-package."
  (find-file "/usr/local/lib/R/3.3/site-library/"))

(defun lix/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
	(set-frame-parameter
	 nil 'alpha
	 (if (eql (cond ((numberp alpha) alpha)
					((numberp (cdr alpha)) (cdr alpha))
					;; Also handle undocumented (<active> <inactive>) form.
					((numberp (cadr alpha)) (cadr alpha)))
			  100)
		 '(70 . 50) '(100 . 100)))))


;;------------------------------------------------------------------------------
;; functions
;;------------------------------------------------------------------------------
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(defun org-block-wrap ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
	  (call-interactively 'org-table-rotate-recalc-marks)
	(let* ((choices '(
					  ("a" . "ASCII")
					  ("c" . "COMMENT")
					  ("C" . "CENTER")
					  ("e" . "EXAMPLE")
					  ("E" . "SRC emacs-lisp")
					  ("h" . "HTML")
					  ("l" . "LaTeX")
					  ("n" . "NOTES")
					  ("q" . "QUOTE")
					  ("s" . "SRC")
					  ("v" . "VERSE")
					  ))
		   (key
			(key-description
			 (vector
			  (read-key
			   (concat (propertize "Template type: " 'face 'minibuffer-prompt)
					   (mapconcat (lambda (choice)
									(concat (propertize (car choice) 'face 'font-lock-type-face)
											": "
											(cdr choice)))
								  choices
								  ", ")))))))
	  (let ((result (assoc key choices)))
		(when result
		  (let ((choice (cdr result)))
			(cond
			 ((region-active-p)
			  (let ((start (region-beginning))
					(end (region-end)))
				(goto-char end)
				(insert "#+END_" choice "\n")
				(goto-char start)
				(insert "#+BEGIN_" choice "\n")))
			 (t
			  (insert "#+BEGIN_" choice "\n")
			  (save-excursion (insert "#+END_" choice))))))))))

(defun formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
	(let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
		   (html (with-current-buffer buf (buffer-string))))
	  (with-current-buffer buf
		(shell-command-on-region
		 (point-min)
		 (point-max)
		 "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
	  (kill-buffer buf))))

(global-set-key (kbd "H-w") 'formatted-copy)

(defun org-remove-headlines (backend)
  "Remove headlines with :no_title: tag."
  (org-map-entries (lambda () (let ((beg (point)))
								(outline-next-visible-heading 1)
								(backward-char)
								(delete-region beg (point))))
				   "no_export" tree)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
				   "no_title"))

(defun move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
	(call-interactively #'write-file)
	(when old-location
	  (delete-file old-location))))

(defun last-search-buffer ()
  "open last helm-ag or hgrep buffer."
  (interactive)
  (cond ((get-buffer "*helm ag results*")
		 (switch-to-buffer-other-window "*helm ag results*"))
		((get-buffer "*helm-ag*")
		 (helm-resume "*helm-ag*"))
		((get-buffer "*hgrep*")
		 (switch-to-buffer-other-window "*hgrep*"))
		(t
		 (message "No previous search buffer found"))))

(defun rotate-windows (count)
  "Rotate your windows.
Dedicated windows are left untouched. Giving a negative prefix
argument takes the kindows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
		 (num-windows (length non-dedicated-windows))
		 (i 0)
		 (step (+ num-windows count)))
	(cond ((not (> num-windows 1))
		   (message "You can't rotate a single window!"))
		  (t
		   (dotimes (counter (- num-windows 1))
			 (let* ((next-i (% (+ step i) num-windows))

					(w1 (elt non-dedicated-windows i))
					(w2 (elt non-dedicated-windows next-i))

					(b1 (window-buffer w1))
					(b2 (window-buffer w2))

					(s1 (window-start w1))
					(s2 (window-start w2)))
			   (set-window-buffer w1 b2)
			   (set-window-buffer w2 b1)
			   (set-window-start w1 s2)
			   (set-window-start w2 s1)
			   (setq i next-i)))))))

(defun rotate-windows-backward (count)
  "Rotate your windows backward."
  (interactive "p")
  (spacemacs/rotate-windows (* -1 count)))

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
		((looking-back "\\s)" 1) (backward-sexp arg))
		;; Now, try to succeed from inside of a bracket
		((looking-at "\\s)") (forward-char) (backward-sexp arg))
		((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun format-date (format)
  (let ((system-time-locale "en_US.UTF-8"))
	(insert (format-time-string format))))

(defun insert-date ()
  (interactive)
  (format-date "%A, %B %d %Y"))

(defun insert-date-and-time ()
  (interactive)
  (format-date "%Y-%m-%d %H:%M:%S"))

(defvar spacemacs-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")
(defvar spacemacs-useful-buffers-regexp '("\\*\\(scratch\\|terminal\.\+\\|ansi-term\\|eshell\\)\\*")
  "Regexp used to define buffers that are useful despite matching
  `spacemacs-useless-buffers-regexp'.")

(defun spacemacs/useless-buffer-p (buffer)
  "Determines if a buffer is useful."
  (let ((buf-paren-major-mode (get (with-current-buffer buffer
									 major-mode)
								   'derived-mode-parent))
		(buf-name (buffer-name buffer)))
	;; first find if useful buffer exists, if so returns nil and don't check for
	;; useless buffers. If no useful buffer is found, check for useless buffers.
	(unless (cl-loop for regexp in spacemacs-useful-buffers-regexp do
					 (when (or (eq buf-paren-major-mode 'comint-mode)
							   (string-match regexp buf-name))
					   (return t)))
	  (cl-loop for regexp in spacemacs-useless-buffers-regexp do
			   (when (string-match regexp buf-name)
				 (return t))))))

(defun spacemacs/next-useful-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
	(next-buffer)
	(while (and (spacemacs/useless-buffer-p (current-buffer))
				(not (eq (current-buffer) start-buffer)))
	  (next-buffer))))

(defun spacemacs/previous-useful-buffer ()
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
	(previous-buffer)
	(while (and (spacemacs/useless-buffer-p (current-buffer))
				(not (eq (current-buffer) start-buffer)))
	  (previous-buffer))))

;; from magnars
(defun spacemacs/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
		(filename (buffer-file-name)))
	(if (not (and filename (file-exists-p filename)))
		(error "Buffer '%s' is not visiting a file!" name)
	  (let ((new-name (read-file-name "New name: " filename)))
		(cond ((get-buffer new-name)
			   (error "A buffer named '%s' already exists!" new-name))
			  (t
			   (let ((dir (file-name-directory new-name)))
				 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
				   (make-directory dir t)))
			   (rename-file filename new-name 1)
			   (rename-buffer new-name)
			   (set-visited-file-name new-name)
			   (set-buffer-modified-p nil)
			   (when (fboundp 'recentf-add-file)
				 (recentf-add-file new-name)
				 (recentf-remove-if-non-kept filename))
			   (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;; from magnars
(defun spacemacs/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
		(buffer (current-buffer))
		(name (buffer-name)))
	(if (not (and filename (file-exists-p filename)))
		(ido-kill-buffer)
	  (when (yes-or-no-p "Are you sure you want to delete this file? ")
		(delete-file filename t)
		(kill-buffer buffer)
		(message "File '%s' successfully removed" filename)))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun spacemacs/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? " (buffer-name)))
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
	(message "Buffers deleted!")))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun spacemacs/show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
	(if file-name
		(progn
		  (message file-name)
		  (kill-new file-name))
	  (error "Buffer not visiting a file"))))

(defun spacemacs/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
	(switch-to-buffer newbuf)))

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun spacemacs/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP. This
  function, unlike the built-in `kill-matching-buffers` does so
  WITHOUT ASKING. The optional second argument indicates whether to
  kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
	(let ((name (buffer-name buffer)))
	  (when (and name (not (string-equal name ""))
				 (or internal-too (/= (aref name 0) ?\s))
				 (string-match regexp name))
		(kill-buffer buffer)))))

;; http://stackoverflow.com/a/10216338/4869
(defun spacemacs/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun spacemacs/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun spacemacs/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

;; Function creating a eshell buffer with fixed frame height
(defun lix/eshell-buffer (&optional height)
  "Create a eshell buffer with fixed height."
  (interactive)
  (setq height 10)
  (let ((orig-win-height (window-height)))
	(when (>= orig-win-height (* height 2))
	  (split-window-below height)
	  (other-window)))
  (let ((newbuf (get-buffer-create "*eshell*"))
		(height-win (window-height))
		(height- (- height (window-height))))
	(eshell)
	(window-resize (selected-window) height-)))

;; Open projects folder
(defun lix/goto-projects ()
  (interactive)
  (find-file "~/projects/"))

(defun lix/goto-home ()
  (interactive)
  (find-file "~"))

;;; describe this point lisp only
(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
	This checks in turn:
	-- for a function name where point is
	-- for a variable name where point is
	-- for a surrounding function call
	"
  (interactive)
  (let (sym)
	;; sigh, function-at-point is too clever.  we want only the first half.
	(cond ((setq sym (ignore-errors
					   (with-syntax-table emacs-lisp-mode-syntax-table
						 (save-excursion
						   (or (not (zerop (skip-syntax-backward "_w")))
							   (eq (char-syntax (char-after (point))) ?w)
							   (eq (char-syntax (char-after (point))) ?_)
							   (forward-sexp -1))
						   (skip-chars-forward "`'")
						   (let ((obj (read (current-buffer))))
							 (and (symbolp obj) (fboundp obj) obj))))))
		   (describe-function sym))
		  ((setq sym (variable-at-point)) (describe-variable sym))
		  ;; now let it operate fully -- i.e. also check the
		  ;; surrounding sexp for a function call.
		  ((setq sym (function-at-point)) (describe-function sym)))))


;;; Remove modeline box
(defun remove-mode-line-box (&rest args)
  (set-face-attribute 'mode-line nil :box nil :underline nil)
  (set-face-attribute 'mode-line-inactive nil :box nil :underline nil))
