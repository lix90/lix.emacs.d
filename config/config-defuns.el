;;; config-defuns.el --- Personal utility functions:

;;; Commentary:

;;; Code:

(defun set-eol-conversion (new-eol)
  "Specify new end-of-line conversion NEW-EOL for the buffer's file
   coding system. This marks the buffer as modified."
  (interactive "SEnd-of-line conversion for visited file: \n")
  ;; Check for valid user input.
  (unless (or (string-equal new-eol "unix")
              (string-equal new-eol "dos")
              (string-equal new-eol "mac"))
    (error "Invalid EOL type, %s" new-eol))
  (if buffer-file-coding-system
      (let ((new-coding-system (coding-system-change-eol-conversion
                                buffer-file-coding-system new-eol)))
        (set-buffer-file-coding-system new-coding-system))
    (let ((new-coding-system (coding-system-change-eol-conversion
                              'undecided new-eol)))
      (set-buffer-file-coding-system new-coding-system)))
  (message "EOL conversion now %s" new-eol))

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

(defun my/toggle-transparency ()
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

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(defun move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
	(call-interactively #'write-file)
	(when old-location
	  (delete-file old-location))))

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

(defun useless-buffer-p (buffer)
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

(defun next-useful-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
	(next-buffer)
	(while (and (useless-buffer-p (current-buffer))
				(not (eq (current-buffer) start-buffer)))
	  (next-buffer))))

(defun previous-useful-buffer ()
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
	(previous-buffer)
	(while (and (useless-buffer-p (current-buffer))
				(not (eq (current-buffer) start-buffer)))
	  (previous-buffer))))

;; from magnars
(defun rename-current-buffer-file ()
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
(defun delete-current-buffer-file ()
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
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? " (buffer-name)))
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
	(message "Buffers deleted!")))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
	(if file-name
		(progn
		  (message file-name)
		  (kill-new file-name))
	  (error "Buffer not visiting a file"))))

(defun new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
	(switch-to-buffer newbuf)))

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun kill-matching-buffers-rudely (regexp &optional internal-too)
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
(defun copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

(defun restore-desktop ()
  "Load the desktop and enable autosaving."
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (desktop-read)
    (desktop-save-mode 1)))

(provide 'config-defuns)
