(use-package exec-path-from-shell :ensure t :defer t :disabled t
  :if (memq window-system '(mac ns))
  :init
  ;; for speeding up startup time
  ;; see: https://github.com/purcell/exec-path-from-shell/issues/36
  (setq exec-path-from-shell-arguments '("-l"))
  ;; Solve warning of setting locale in ESS
  ;; from: https://stat.ethz.ch/pipermail/r-sig-mac/2015-October/011701.html
  ;; (exec-path-from-shell-copy-envs '("LC_ALL", "LANG"))
  (setq exec-path-from-shell-variables '("LC_ALL" "LANG"))
  (setq exec-path-from-shell-debug t)
  (exec-path-from-shell-initialize))

(unless (getenv "LANG") (setenv "LANG" "en_US.UTF-8"))
(unless (getenv "LC_ALL") (setenv "LC_ALL" "en_US.UTF-8"))
(setenv "MANPATH" (getenv "MANPATH"))
(setq exec-path '(
                  "/Users/lix/anaconda3/bin/"
                  "/usr/local/bin"
                  "/Users/lix/.rvm/gems/ruby-2.3.0/bin"
                  "/Users/lix/.rvm/gems/ruby-2.3.0@global/bin"
                  "/Users/lix/.rvm/bin"
                  "/Applications/Emacs.app/Contents/MacOS/bin"
                  "/usr/bin"
                  "/bin"
                  "/usr/sbin"
                  "/sbin"
                  ))

(setenv "PATH" (mapconcat 'identity exec-path ":"))
;;(setenv "PATH" "/Users/lix/anaconda3/bin/:/usr/local/bin:/Users/lix/.rvm/gems/ruby-2.3.0/bin:/Users/lix/.rvm/gems/ruby-2.3.0@global/bin:/Users/lix/.rvm/bin:/Applications/Emacs.app/Contents/MacOS/bin:/usr/bin:/bin:/usr/sbin:/sbin")

(provide 'config-environment)
