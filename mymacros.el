;; will switch to the shell and run the last comand (M-p)
(fset 'run-last-command
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 98 42 115 104 101 108 108 42 return 134217840 return] 0 "%d")) arg)))

(global-set-key (kbd "\C-x t") 'run-last-command)


(fset 'translate-this
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("_t(\346\346\")" 0 "%d")) arg)))

(global-set-key (kbd "\C-ct") 'translate-this)

;; proxy property
(fset 'proxyproperty
   [?\C-  ?\C-s ?: ?\' ?\C-m ?\C-w ?\C-s ?\' ?\C-? ?\' ?\C-m backspace ?\C-k ?\C-  ?\C-a ?\M-w ?\C-e ?  ?= ?  ?P ?R ?o backspace backspace ?r ?o ?x ?y ?P ?r ?o ?p ?e ?r ?t ?y ?\( ?\' ?\C-y ?\' ?\) ?\C-n ?\C-a tab])


