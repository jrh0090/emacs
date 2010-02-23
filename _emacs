
;; local directory of plugins
(add-to-list 'load-path "~/emacs")

(load "funcs.el")
(load "environment.el")
(load "libraries.el")
(load "zenoss-config.el")
(load "shortcuts.el")
(load "php-config.el")
(load "python-config.el")
(load "javascript-config.el")

;; always start with a shell
(shell)

;; reopen session when opening emacs
;; do this last to make sure all of our major modes are loaded
(desktop-save-mode t)

