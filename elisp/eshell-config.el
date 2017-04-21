(require 'eshell)

;; git-info
(defcustom dotemacs-eshell/prompt-git-info
  t
  "Turns on additional git information in the prompt."
  :group 'dotemacs-eshell
  :type 'boolean)

;; (epe-colorize-with-face "abc" 'font-lock-comment-face)
(defmacro epe-colorize-with-face (str face)
  `(propertize ,str 'face ,face))

(setq eshell-prompt-function
      (lambda ()
        (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
                (when (and dotemacs-eshell/prompt-git-info
                           (fboundp #'vc-git-branches))
                  (let ((branch (car (vc-git-branches))))
                    (when branch
                      (concat
                       (propertize " [" 'face 'font-lock-keyword-face)
                       (propertize branch 'face 'font-lock-function-name-face)
                       (let* ((status (shell-command-to-string "git status --porcelain"))
                              (parts (split-string status "\n" t " "))
                              (states (mapcar #'string-to-char parts))
                              (added (count-if (lambda (char) (= char ?A)) states))
                              (modified (count-if (lambda (char) (= char ?M)) states))
                              (deleted (count-if (lambda (char) (= char ?D)) states)))
                         (when (> (+ added modified deleted) 0)
                           (propertize (format " +%d ~%d -%d" added modified deleted) 'face 'font-lock-comment-face)))
                       (propertize "]" 'face 'font-lock-keyword-face)))))
                (when (and (boundp #'venv-current-name) venv-current-name)
                  (concat
                   (epe-colorize-with-face " [" 'epe-venv-face)
                   (propertize venv-current-name 'face `(:foreground "#2E8B57" :slant italic))
                   (epe-colorize-with-face "]" 'epe-venv-face)))
                (propertize " $ " 'face 'font-lock-constant-face))))


;; retrieved from:
;; http://emacs.stackexchange.com/questions/7617/how-to-programmatically-execute-a-command-in-eshell
;; customed function
(defun eshell-send-input (&optional use-region queue-p no-newline input-string-a)
  "Send the input received to Eshell for parsing and processing.
After `eshell-last-output-end', sends all text from that marker to
point as input.  Before that marker, calls `eshell-get-old-input' to
retrieve old input, copies it to the end of the buffer, and sends it.
-  If USE-REGION is non-nil, the current region (between point and mark)
will be used as input.
-  If QUEUE-P is non-nil, input will be queued until the next prompt,
rather than sent to the currently active process.  If no process, the
input is processed immediately.
-  If NO-NEWLINE is non-nil, the input is sent without an implied final
newline."
  (interactive "P")
  ;; Note that the input string does not include its terminal newline.
  (let ((proc-running-p
         (and (eshell-interactive-process)
              (not queue-p)))
        (inhibit-point-motion-hooks t)
        after-change-functions)
    (unless (and proc-running-p
                 (not (eq (process-status (eshell-interactive-process)) 'run)))
      (if (or proc-running-p
              (>= (point) eshell-last-output-end))
          (goto-char (point-max))
        ;; This is for a situation when point is before `point-max'.
        (let ((copy (or input-string-a (eshell-get-old-input use-region))))
          (goto-char eshell-last-output-end)
          (insert-and-inherit copy)))
      (unless (or no-newline
                  (and eshell-send-direct-to-subprocesses
                       proc-running-p))
        (insert-before-markers-and-inherit ?\n))
      (if proc-running-p
          (progn
            (eshell-update-markers eshell-last-output-end)
            (if (or eshell-send-direct-to-subprocesses
                    (= eshell-last-input-start eshell-last-input-end))
                (unless no-newline
                  (process-send-string (eshell-interactive-process) "\n"))
              (process-send-region (eshell-interactive-process)
                                   eshell-last-input-start
                                   eshell-last-input-end)))
        (if (and (null input-string-a) (= eshell-last-output-end (point)))
            ;; This next line is for a situation when nothing is there --
            ;; i.e., just make a new command prompt.
            (run-hooks 'eshell-post-command-hook)
          (let (input)
            (eshell-condition-case err
                (progn
                  (setq input (or input-string-a
                                  (buffer-substring-no-properties
                                   eshell-last-output-end (1- (point)))))
                  (run-hook-with-args 'eshell-expand-input-functions
                                      eshell-last-output-end (1- (point)))
                  (let ((cmd (eshell-parse-command-input
                              eshell-last-output-end (1- (point)) nil input-string-a)))
                    (when cmd
                      (eshell-update-markers eshell-last-output-end)
                      (setq input (buffer-substring-no-properties
                                   eshell-last-input-start
                                   (1- eshell-last-input-end)))
                      (run-hooks 'eshell-input-filter-functions)
                      (and (catch 'eshell-terminal
                             (ignore
                              (if (eshell-invoke-directly cmd)
                                  (eval cmd)
                                (eshell-eval-command cmd input))))
                           (eshell-life-is-too-much)))))
              (quit
               (eshell-reset t)
               (run-hooks 'eshell-post-command-hook)
               (signal 'quit nil))
              (error
               (eshell-reset t)
               (eshell-interactive-print
                (concat (error-message-string err) "\n"))
               (run-hooks 'eshell-post-command-hook)
               (insert-and-inherit input)))))))))

(defun eshell-parse-command-input (beg end &optional args input-string-b)
  "Parse the command input from BEG to END.
The difference is that `eshell-parse-command' expects a complete
command string (and will error if it doesn't get one), whereas this
function will inform the caller whether more input is required.
-  If nil is returned, more input is necessary (probably because a
multi-line input string wasn't terminated properly).  Otherwise, it
will return the parsed command."
  (let (delim command)
    (if (setq delim (catch 'eshell-incomplete
                      (ignore
                       (setq command
                             (eshell-parse-command
                              (cons beg end) args t input-string-b)))))
        (ignore
         (message "Expecting completion of delimiter %c ..."
                  (if (listp delim)
                      (car delim)
                    delim)))
      command)))

(defun eshell-parse-command (command &optional args toplevel input-string-c)
  "Parse the COMMAND, adding ARGS if given.
COMMAND can either be a string, or a cons cell demarcating a buffer
region.  TOPLEVEL, if non-nil, means that the outermost command (the
user's input command) is being parsed, and that pre and post command
hooks should be run before and after the command."
  (let* (
         eshell--sep-terms
         (terms
          (if input-string-c
              (eshell-parse-arguments--temp-buffer input-string-c)
            (append
             (if (consp command)
                 (eshell-parse-arguments (car command) (cdr command))
               (let ((here (point))
                     (inhibit-point-motion-hooks t))
                 (with-silent-modifications
                   ;; FIXME: Why not use a temporary buffer and avoid this
                   ;; "insert&delete" business?  --Stef
                   (insert command)
                   (prog1
                       (eshell-parse-arguments here (point))
                     (delete-region here (point))))))
             args)))
         (commands
          (mapcar
           (function
            (lambda (cmd)
              (setq cmd (if (or (not (car eshell--sep-terms))
                                (string= (car eshell--sep-terms) ";"))
                            (eshell-parse-pipeline cmd)
                          `(eshell-do-subjob
                            (list ,(eshell-parse-pipeline cmd)))))
              (setq eshell--sep-terms (cdr eshell--sep-terms))
              (if eshell-in-pipeline-p
                  cmd
                `(eshell-trap-errors ,cmd))))
           (eshell-separate-commands terms "[&;]" nil 'eshell--sep-terms))) )
    (let ((cmd commands))
      (while cmd
        (if (cdr cmd)
            (setcar cmd `(eshell-commands ,(car cmd))))
        (setq cmd (cdr cmd))))
    (if toplevel
        `(eshell-commands (progn
                            (run-hooks 'eshell-pre-command-hook)
                            (catch 'top-level (progn ,@commands))
                            (run-hooks 'eshell-post-command-hook)))
      (macroexp-progn commands))))

(defun eshell-parse-arguments--temp-buffer (input-string-d)
  "Parse all of the arguments at point from BEG to END.
Returns the list of arguments in their raw form.
Point is left at the end of the arguments."
  (with-temp-buffer
    (insert input-string-d)
    (let ((inhibit-point-motion-hooks t)
          (args (list t))
          delim)
      (with-silent-modifications
        (remove-text-properties (point-min) (point-max)
                                '(arg-begin nil arg-end nil))
        (goto-char (point-min))
        (if (setq
             delim
             (catch 'eshell-incomplete
               (while (not (eobp))
                 (let* ((here (point))
                        (arg (eshell-parse-argument)))
                   (if (= (point) here)
                       (error "Failed to parse argument '%s'"
                              (buffer-substring here (point-max))))
                   (and arg (nconc args (list arg)))))))
            (throw 'eshell-incomplete (if (listp delim)
                                          delim
                                        (list delim (point) (cdr args)))))
        (cdr args)))))

;; retrieved from:
;; http://emacs.stackexchange.com/questions/13579/how-to-open-shell-or-eshell-in-a-new-window-or-frame
(defun eshell-other-window ()
  "Open a `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(provide 'eshell-config)
