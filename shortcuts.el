;; Alias ctrl x ctrl m for alt m
(define-key global-map "\C-x\C-p" 'revert-buffer)
(define-key global-map "\M-g" 'goto-line)

;; control a to begining of statement instead of line
(global-set-key (kbd "C-a") 'back-to-indentation)

;; make ctrl z undo, as well as ctrl x ctrl u
(global-set-key "\C-z" 'undo)
(global-set-key "\C-x\C-u" 'undo)

;; auto indent after new line
(define-key global-map (kbd "RET") 'newline-and-indent)

;; short cut to shell
(global-set-key (kbd "\C-c 5") 'shell)

;; shortcuts i like
(global-set-key (kbd "\C-c \C-t") 'lisp-complete-symbol)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "<f5>") 'rgrep)