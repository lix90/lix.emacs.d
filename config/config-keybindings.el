(when is-mac
  (global-set-key (kbd "<H-backspace>") 'delete-forward-char)
  ;; Keybindings
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease)
  (global-set-key (kbd "s-0") 'text-scale-adjust)
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "s-v") 'yank) 
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "s-s")
				  (lambda ()
					(interactive)
					(call-interactively (key-binding "\C-x\C-s")))))

;; Show which-key top-level bindings
;; override evil insert for kill line
;;(general-define-key :states '(insert) "C-k" 'kill-line)

(global-set-key (kbd "C-,") 'set-mark-command)
(global-set-key (kbd "C-.") 'comment-or-uncomment-region)
(global-set-key (kbd "H-i") 'ivy-immediate-done)
(global-set-key (kbd "H-k") 'which-key-show-top-level)
(global-set-key (kbd "H-9") 'shrink-window-horizontally)
(global-set-key (kbd "H-0") 'enlarge-window-horizontally)
(global-set-key (kbd "H--") 'shrink-window)
(global-set-key (kbd "H-=") 'enlarge-window)

;; Quick
(leader-key
 "<SPC>" 'counsel-M-x
 "h" 'ivy-resume
 "i" 'ivy-imenu-anywhere
 "r" 'counsel-recentf
 "M" 'woman
 "." 'quick-commit
 ";" 'comment-or-uncomment-region
 "TAB" 'switch-to-previous-buffer
 "D" 'lix/restore-desktop
 "O" 'counsel-osx-app
 ;; window numbering
 "0" 'winum-select-window-0
 "1" 'winum-select-window-1
 "2" 'winum-select-window-2
 "3" 'winum-select-window-3
 "4" 'winum-select-window-4
 "5" 'winum-select-window-5
 "6" 'winum-select-window-6
 "7" 'winum-select-window-7
 "8" 'winum-select-window-8
 "9" 'winum-select-window-9
 )

;;; IDE

(defalias 'run-matlab 'matlab-shell)
(defalias 'run-eshell 'eshell)
(defalias 'run-shell 'shell)
(defalias 'run-R 'R)
(defalias 'run-elisp 'ielm)
(defalias 'run-clisp 'slime)

(defun run-python2()
  (interactive)
  (lix/switch-to-python2)
  (run-python))
(defun run-python3()
  (interactive)
  (lix/switch-to-python3)
  (run-python))

(leader-key
 "I" '(:ignore t :which-key "IDE & REPL")
 "Ir" 'run-R 
 "Ie" 'run-eshell
 "Is" 'run-shell
 "Ij" 'nodejs-repl
 "Ii" 'run-elisp
 "Ic" 'run-clisp
 "Im" 'run-matlab
 "Ip" 'run-python
 "I2" 'run-python2
 "I3" 'run-python3
 )

(leader-key
 "d" '(:ignore t :which-key "Dictionary")
 "db" 'Bing-dictionary
 "dy" 'Youdao-dictionary
 "ds" 'Stardict)

(leader-key
 "b"  '(:ignore t :which-key "Buffers") 
 "bb" 'ivy-switch-buffer 
 "bd" 'kill-this-buffer
 "bD" 'kill-buffer-and-window
 "be" 'eval-buffer-until-error
 "bf" 'reveal-in-osx-finder
 "bk" 'kill-buffer
 "bK" 'spacemacs/kill-other-buffers
 "bn" 'spacemacs/new-empty-buffer 
 "br" 'revert-buffer
 "bR" 'spacemacs/rename-current-buffer-file
 "bt" 'open-dir-in-iterm
 "b." 'comint-clear-buffer
 "b[" 'spacemacs/previous-useful-buffer
 "b]" 'spacemacs/next-useful-buffer
 )

(leader-key
 "f"  '(:ignore t :which-key "Files")
 "ff" 'counsel-find-file
 "fl" 'counsel-locate
 "fj" 'counsel-file-jump 
 "fo" 'crux-open-with
 "fs" 'save-buffer
 "fr" 'counsel-recentf
 "fy" 'spacemacs/show-and-copy-buffer-filename
 "fH" 'lix/goto-home
 "fP" 'lix/goto-projects
 "fC" 'lix/goto-config
 "fi" 'lix/open-init.el
 "fc" 'lix/open-custom.el
 )

(leader-key
 "g"  '(:ignore t :which-key "Git")
 "gb" 'magit-blame
 "gc" 'magit-commit
 "gd" 'magit-diff
 "gl" 'magit-log
 "gr" 'magit-reflog
 "gs" 'magit-status
 )

(leader-key
 "n" '(:ignore t :which-key "Navigation") 
 "n(" 'forward-or-backward-sexp
 ;; Symbol
 "nf" 'sp-forward-symbol
 "nb" 'sp-backward-symbol
 ;; Move in backward and forward
 "n," 'sp-backward-slurp-sexp
 "n." 'sp-forward-slurp-sexp
 ;; Move out backward and forward
 "n<" 'sp-backward-barf-sexp
 "n>" 'sp-forward-barf-sexp
 ;; sexp
 "ns" 'sp-splice-sexp
 "nu" 'sp-up-sexp
 "nd" 'sp-down-sexp
 "nn" 'sp-next-sexp
 "np" 'sp-previous-sexp
 "nl" 'sp-forward-sexp
 "nh" 'sp-backward-sexp 
 ;; goto
 "n\\" 'goto-last-change)

(bind-key "H-F" 'sp-forward-symbol)
(bind-key "H-B" 'sp-backward-symbol)
(bind-key "H-)" 'sp-forward-sexp)
(bind-key "H-(" 'sp-backward-sexp)
(bind-key "H-]" 'sp-next-sexp)
(bind-key "H-[" 'sp-previous-sexp)

;; m ==> markdown
;; o ==> org

(leader-key
 "P" '(:ignore t :which-key "Packages")
 "Pl" 'paradox-list-packages
 "Pu" 'paradox-upgrade-packages
 "Pn" 'package-utils-upgrade-by-name
 "Pr" 'package-refresh-contents
 )

(leader-key
 "p" '(:ignore t :which-key "Projects")
 "pf" 'counsel-projectile-find-file
 "pd" 'counsel-projectile-find-dir
 "pb" 'counsel-projectile-switch-to-buffer
 "pp" 'counsel-projectile-switch-project
 "pn" 'neotree-toggle
 )

(leader-key
 "q"  '(:ignore t :which-key "Quit")
 "qq" 'save-desktop-save-buffers-kill-emacs
 "qQ" 'evil-quit-all
 "qr" 'restart-emacs
 )

(leader-key
 "s" '(:ignore t :which-key "Search")
 "sC" '(:ignore t :which-key "Colors") 

 ;; avy
 "sw" 'avy-goto-word-or-subword-1
 "sl" 'avy-goto-line
 "sc" 'avy-goto-char
 ;; multi-cursor
 "sa" 'mc/mark-all-like-this
 "sA" 'mc/mark-all-words-like-this
 "sn" 'mc/mark-next-like-this
 "sN" 'mc/mark-previous-like-this
 "sr" 'phi-rectangle-set-mark-command
 "sR" 'phi-rectangle-kill-region
 ;;
 "s." 'apropos
 "sf" 'describe-foo-at-point
 "sF" 'find-function-at-point
 "s?" 'dash-at-point
 "sd" 'dash-at-point-with-docset
 "sg" 'counsel-dash
 "sd" 'counsel-dash-at-point

 ;; Counsel
 "sh" 'counsel-command-history
 "sl" 'counsel-find-library
 "sk" 'counsel-descbinds
 "s/" 'counsel-describe-face

 ;; Colors
 "sCe" 'counsel-colors-emacs
 "sCw" 'counsel-colors-web
 )

(leader-key
 "t"  '(:ignore t :which-key "Toggles")
 "ta" 'company-mode
 "t!" 'flycheck-mode
 "th" 'hl-line-mode
 "tn" 'linum-mode 
 "tr" 'rainbow-identifiers-mode 
 "tR" 'rainbow-mode
 "tt" 'counsel-load-theme
 "tT" 'lix/toggle-transparency 
 "tp" 'smartparens-strict-mode
 "tc" 'centered-cursor-mode
 "tC" 'centered-window-mode
 "td" 'distraction-free 
 "tw" 'writeroom-mode 
 "tf" 'flyspell-mode 
 "tF" 'toggle-frame-fullscreen
 "tg" 'git-gutter+-mode
 "tx" 'nocomments-mode
 )

(leader-key
 "w"  '(:ignore t :which-key "Windows")
 ;; Delete window
 "wc" 'delete-window
 "wC" 'delete-other-windows
 "wh" 'delete-window-left
 "wl" 'delete-window-right
 "wj" 'delete-window-below
 "wk" 'delete-window-above
 ;; Split window
 "w|" 'split-window-right-and-focus
 "w-" 'split-window-below-and-focus 
 "ws" 'evil-window-split
 "wv" 'evil-window-vsplit
 ;; Rotate window
 "wr" 'rotate-windows
 "wR" 'rotate-windows-backward
 )

;;; Key configuration

(defun my-markdown-config ()
  "Modify keymaps in markdown mode"
  (leader-key
   "m"   '(:ignore t :which-key "Markdown")
   "mc"  '(:ignore t :which-key "command")
   "mh"  '(:ignore t :which-key "insert")
   "mi"  '(:ignore t :which-key "lists")
   "mx"  '(:ignore t :which-key "text")
   ;; Movement
   "m{"   'markdown-backward-paragraph
   "m}"   'markdown-forward-paragraph
   ;; Completion, and Cycling
   "m]"   'markdown-complete
   ;; Indentation
   "m>"   'markdown-indent-region
   "m<"   'markdown-exdent-region
   ;; Buffer-wide commands
   "mc]"  'markdown-complete-buffer
   "mcc"  'markdown-check-refs
   "mce"  'markdown-export
   "mcm"  'markdown-other-window
   "mcn"  'markdown-cleanup-list-numbers
   "mco"  'markdown-open
   "mcp"  'markdown-preview
   "mcv"  'markdown-export-and-preview
   "mcw"  'markdown-kill-ring-save
   ;; headings
   "mhi"  'markdown-insert-header-dwim
   "mhI"  'markdown-insert-header-setext-dwim
   "mh1"  'markdown-insert-header-atx-1
   "mh2"  'markdown-insert-header-atx-2
   "mh3"  'markdown-insert-header-atx-3
   "mh4"  'markdown-insert-header-atx-4
   "mh5"  'markdown-insert-header-atx-5
   "mh6"  'markdown-insert-header-atx-6
   "mh!"  'markdown-insert-header-setext-1
   "mh@"  'markdown-insert-header-setext-2
   ;; Insertion of common elements
   "m-"   'markdown-insert-hr
   "mif"  'markdown-insert-footnote
   "mii"  'markdown-insert-image
   "mik"  'spacemacs/insert-keybinding-markdown
   "miI"  'markdown-insert-reference-image
   "mil"  'markdown-insert-link
   "miL"  'markdown-insert-reference-link-dwim
   "miw"  'markdown-insert-wiki-link
   "miu"  'markdown-insert-uri
   ;; Element removal
   "mk"   'markdown-kill-thing-at-point
   ;; List editing
   "mli"  'markdown-insert-list-item
   ;; region manipulation
   "mxb"  'markdown-insert-bold
   "mxi"  'markdown-insert-italic
   "mxc"  'markdown-insert-code
   "mxC"  'markdown-insert-gfm-code-block
   "mxq"  'markdown-insert-blockquote
   "mxQ"  'markdown-blockquote-region
   "mxp"  'markdown-insert-pre
   "mxP"  'markdown-pre-region
   ;; Following and Jumping
   "mN"   'markdown-next-link
   "mf"   'markdown-follow-thing-at-point
   "mP"   'markdown-previous-link
   "<RET>" 'markdown-jump
   ))

(eval-after-load 'markdown-mode '(my-markdown-config))

(defun my-org-config()
  (leader-key
   "o"  '(:ignore t :which-key "Org")
   "oh" '(:ignore t :which-key "headers")
   "oi" '(:ignore t :which-key "insert")
   "oS" '(:ignore t :which-key "subtree")
   "ot" '(:ignore t :which-key "tables")
   "or" '(:ignore t :which-key "org-reveal")
   "oj" 'cpm/org-journal
   "oc" 'org-capture
   "of" 'org-footnote-action
   "oP" 'org-set-property
   ;; "P" 'org-publish-current-project
   "op" 'org-publish-current-file
   "o:" 'org-set-tags
   "oa" 'org-agenda
   "ob" 'org-tree-to-indirect-buffer
   "oA" 'org-archive-subtree
   "ol" 'org-open-at-point
   "oT" 'org-show-todo-tree

   "orr" 'org-reveal-export-to-html-and-browse
   "ors" 'org-reveal-export-current-subtree
   "orp" 'reveal-to-pdf

   "o." 'org-time-stamp
   "o!" 'org-time-stamp-inactive

   ;; headings
   "ohi" 'org-insert-heading-after-current
   "ohI" 'org-insert-heading

   ;; More cycling options (timestamps, headlines, items, properties)
   "oL" 'org-shiftright
   "oH" 'org-shiftleft
   "oJ" 'org-shiftdown
   "oK" 'org-shiftup

   ;; Subtree editing
   "oSl" 'org-demote-subtree
   "oSh" 'org-promote-subtree
   "oSj" 'org-move-subtree-down
   "oSk" 'org-move-subtree-up

   ;; tables
   "ota" 'org-table-align
   "otb" 'org-table-blank-field
   "otc" 'org-table-convert
   "otdc" 'org-table-delete-column
   "otdr" 'org-table-kill-row
   "ote" 'org-table-eval-formula
   "otE" 'org-table-export
   "oth" 'org-table-previous-field
   "otH" 'org-table-move-column-left
   "otic" 'org-table-insert-column
   "otih" 'org-table-insert-hline
   "otiH" 'org-table-hline-and-move
   "otir" 'org-table-insert-row
   "otI" 'org-table-import
   "otj" 'org-table-next-row
   "otJ" 'org-table-move-row-down
   "otK" 'org-table-move-row-up
   "otl" 'org-table-next-field
   "otL" 'org-table-move-column-right
   "otn" 'org-table-create
   "otN" 'org-table-create-with-table.el
   "otr" 'org-table-recalculate
   "ots" 'org-table-sort-lines
   "ottf" 'org-table-toggle-formula-debugger
   "otto" 'org-table-toggle-coordinate-overlays
   "otw" 'org-table-wrap-region

   ;; Multi-purpose keys
   ;; "o*" 'org-ctrl-c-star
   ;; "oRET" 'org-ctrl-c-ret
   "o-" 'org-ctrl-c-minus
   "o^" 'org-sort
   "o/" 'org-sparse-tree

   "oI" 'org-clock-in
   "on" 'org-narrow-to-subtree
   "oN" 'widen
   "oO" 'org-clock-out
   "oq" 'org-clock-cancel
   "oR" 'org-refile
   "os" 'org-schedule

   ;; insertion of common elements
   "oil" 'org-insert-link
   "oif" 'org-footnote-new
   ))

(eval-after-load 'org '(my-org-config))

(provide 'config-keybindings)
