(leader-key
 "<SPC>" 'counsel-M-x
 "h" 'ivy-resume
 "i" 'ivy-imenu-anywhere
 "d" 'my-desktop
 "M" 'woman
 "'" 'shell
 "." 'quick-commit
 ";" 'evil-commentary-line
 "[" 'spacemacs/previous-useful-buffer
 "]" 'spacemacs/next-useful-buffer
 "TAB" 'switch-to-previous-buffer
 )

;;
;; IDE
;;

(defalias 'run-matlab 'matlab-shell)
(defalias 'run-eshell 'eshell)
(defalias 'run-shell 'shell)
(defalias 'run-R 'R)
(defalias 'run-elisp 'ielm)

(leader-key
 "I" '(:ignore t :which-key "IDE")
 "Ir" 'run-R
 "Ip" 'run-python
 "Ie" 'run-eshell
 "Is" 'run-shell
 "Ij" 'run-js
 "Ii" 'run-elisp
 "Im" 'run-matlab
 "I2" 'switch-to-python2
 "I3" 'switch-to-python3
 )

(leader-key
 "b"  '(:ignore t :which-key "Buffers")
 "bc" 'spacemacs/new-empty-buffer
 "bD" 'kill-buffer-and-window
 "bd" 'kill-this-buffer
 "bf" 'reveal-in-osx-finder
 "bj" 'jump-in-buffer
 "bk" 'evil-delete-buffer
 "bK" 'spacemacs/kill-other-buffers
 "bn" 'spacemacs/new-empty-buffer
 "bN" 'nuke-all-buffers
 "br" 'revert-buffer
 "bR" 'spacemacs/rename-current-buffer-file
 "bt" 'open-dir-in-iterm
 )

(leader-key
 "c"  '(:ignore t :which-key "Commenting")
 "cb" 'org-block-wrap
 "cc" 'evil-commentary
 "cl" 'evil-commentary-line
 "cy" 'evil-commentary-yank-line
 )

(leader-key
 "D" '(:ignore t :which-key "Documentations")
 "Da" 'apropos
 "D?" 'counsel-descbinds
 "Dd" 'describe-foo-at-point
 "DD" 'find-function-at-point
 "Dk" 'describe-keymap
 "DK" 'describe-key
 "Dm" 'describe-mode
 "Dv" 'describe-variable
 "Dc" 'counsel-colors-emacs
 "DC" 'counsel-colors-web
 ;; Snippets
 "Ds" 'yas-insert-snippet
 "Dn" 'yas-new-snippet
 "Df" 'yas-visit-snippet-file
 )

(leader-key
 "f"  '(:ignore t :which-key "Files")
 "ff" 'counsel-find-file
 "fl" 'counsel-locate
 "fo" 'crux-open-with
 "fs" 'save-buffer
 "fr" 'counsel-recentf
 "fy" 'spacemacs/show-and-copy-buffer-filename
 "fh" 'lix/goto-home
 "fp" 'lix/goto-projects
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

;; m ==> markdown
;; o ==> org

;; Show which-key top-level bindings
(global-set-key (kbd "H-k") 'which-key-show-top-level)
;; override evil insert for kill line
(general-define-key :states '(insert) "C-k" 'kill-line)

(leader-key
 "P" '(:ignore t :which-key "Packages")
 "Pl" 'paradox-list-packages
 "Pu" 'paradox-upgrade-packages
 "Pn" 'package-utils-upgrade-by-name
 )

(leader-key
 "p" '(:ignore t :which-key "Projects")
 ;; "p!"  'projectile-run-shell-command-in-root
 ;; "p&"  'projectile-run-async-shell-command-in-root
 ;; "pa"  'projectile-toggle-between-implementation-and-test
 ;; "pb"  'helm-projectile-switch-to-buffer
 ;; "pc"  'projectile-compile-project
 ;; "pd"  'helm-projectile-find-dir
 ;; "pD"  'projectile-dired
 ;; "pf"  'helm-projectile-find-file
 ;; "pg"  'goto-projects
 ;; "ph"  'helm-projectile
 ;; "pG"  'projectile-regenerate-tags
 ;; "pI"  'projectile-invalidate-cache
 ;; "pk"  'projectile-kill-buffers
 ;; "po"  'projectile-multi-occur
 ;; "pp"  'helm-projectile-switch-project
 ;; "pr"  'helm-projectile-recentf
 ;; "pR"  'projectile-replace
 ;; "ps"  'bmkp-set-desktop-bookmark
 ;; "pS"  'bmkp-desktop-jump
 ;; "pT"  'projectile-find-test-file
 ;; "pv"  'projectile-vc
 ;; "py"  'projectile-find-tag
 )


(leader-key
 "q"  '(:ignore t :which-key "Quit")
 "qq" 'save-desktop-save-buffers-kill-emacs
 "qQ" 'evil-quit-all
 "qr" 'restart-emacs
 )

(leader-key
 "s" '(:ignore t :which-key "Search")
 "sb" 'bing-dict-brief
 "sj" 'forward-or-backward-sexp
 "ss" 'swiper
 ;; avy
 "sw" 'avy-goto-word-or-subword-1
 "sl" 'avy-goto-line
 "sc" 'avy-goto-char
 ;; multi-cursor
 "sa" 'mc/mark-all-like-this
 "sA" 'mc/mark-all-words-like-this
 "sn" 'mc/mark-next-like-this
 "sN" 'mc/mark-previous-like-this
 )

(leader-key
 "t"  '(:ignore t :which-key "Toggles")
 "ta" 'company-mode
 "tb" 'fancy-battery-mode
 "tc" 'centered-cursor-mode
 "tC" 'centered-window-mode
 "td" 'distraction-free
 "tf" 'toggle-serif
 "tF" 'toggle-frame-fullscreen
 "tg" 'git-gutter+-mode
 "th" 'hl-line-mode
 "te" 'toggle-indicate-empty-lines
 "tE" 'eldoc-mode
 "tm" 'hidden-mode-line-mode
 "tM" 'spaceline-toggle-minor-modes
 "tn" 'linum-mode
 "tN" 'neotree-toggle
 "to" 'org-toggle-link-display
 "tp" 'smartparens-mode
 "tr" 'rainbow-identifiers-mode
 "tR" 'rainbow-mode
 ;;"ts" 'toggle-dark-light-theme
 "ts" 'flyspell-mode
 "tw" 'writeroom-mode
 "tt" 'counsel-load-theme
 "tT" 'lix--toggle-transparency
 )

(leader-key
 "u"  '(:ignore t :which-key "User")
 "um" 'cpm/org-to-markdown
 "uc" 'pandoc-convert-to-pdf
 "uo" 'cpm/markdown-to-org
 "up" 'run-pandoc
 "uP" 'pandoc-pdf-open
 "us" 'sb-expand-current-file
 "uS" 'just-one-space
 "ud" 'distraction-free
 "uD" 'my-desktop
 "uj" 'goto-journal
 "uw" 'count-words
 )

(leader-key
 "w"  '(:ignore t :which-key "Windows")
 "wc" 'delete-window
 "wC" 'delete-other-windows
 "w-" 'evil-window-split
 "wv" 'evil-window-vsplit
 "wr" 'rotate-windows
 "wR" 'rotate-windows-backward
 )

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
