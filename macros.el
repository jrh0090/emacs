;; will switch to the shell and run the last comand (M-p)
(fset 'run-last-command
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 98 42 115 104 101 108 108 42 return 134217840 return] 0 "%d")) arg)))

(global-set-key (kbd "\C-x t") 'run-last-command)
