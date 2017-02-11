;;; Useful functions


;; Function creating a eshell buffer with fixed frame height
(defun lix--eshell-buffer (&optional height)
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
