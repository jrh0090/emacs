;; lightweight javascript editing mode
;; this is useful when there is javascript and html in the same page
(load "espresso")

;; js2 javascript IDE
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; jslint settings
(defvar jslint-global-vars "/*global Ext, Zenoss, _t*/")
(defvar jslint-global-config "/*jslint browser:true, devel:true, nomen:false, white:false, onevar:true, eqeqeq:false, plusplus:false*/"
   "All of the options to pass into jslint, see the http://www.jslint.com/lint.html for a list of them all")

(defun jslint-current-buffer ()
  "This will run jslint in the foreground on the current file you are working on
and display the results in the other window. Since JSlint is so slow with rhino this
is good to use to check your work not to use with flymake. Honestly jslint is far to picky
to run in flymake mode so I think this is a better option"
  (interactive)
  (when (buffer-file-name)
    (save-excursion 
      ;; goto the begining of the buffer and place the magic keywords for the js file
      (goto-char (point-min))
      (insert jslint-global-config)
      (insert "\n")
      (insert jslint-global-vars)
      (insert "\n")
      (save-buffer)
      
      ;; actually run the command on the file
      (shell-command
       (concat "java org.mozilla.javascript.tools.shell.Main ~/Library/JSLint/jslint.js "
               (buffer-file-name))
       "JSLint Output")
      (switch-to-buffer-other-window "JSLint Output")
      
      ;; now delete the global vars declaration
      (other-window 1)
      (goto-char (point-min))
      (kill-line 2)
      (save-buffer))))

;; jscomint inferior process and set up files
(require 'js-comint)
(add-hook 'js2-mode-hook '(lambda () 
             (local-set-key "\C-x\C-e" 'js-send-last-sexp)
             (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
             (local-set-key "\C-cb" 'js-send-buffer)
             (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
             (local-set-key "\C-c\C-r" 'js-send-region-and-go)
             (local-set-key "\C-cl" 'js-load-file-and-go)
             (local-set-key "\C-c\C-z" 'run-js)
             ;; js2 ignores some commands
             (local-set-key (kbd "RET") 'newline-and-indent)
             (local-set-key "\C-a" 'back-to-indentation)
             (local-set-key (kbd "\C-c i") 'jslint-current-buffer)
             (drag-stuff-mode t)
             (setq js2-bounce-indent-p nil)
             (yas/minor-mode-on)
             ))

;; for jscomint, tells it where my js file is
(setq inferior-js-program-command "java org.mozilla.javascript.tools.shell.Main")

;; list of my JS libraries I need to load by default.
(defvar js-comint-libraries
  '("/Users/joseph/Library/JavaScript/env.js"
   "/Users/joseph/zenoss/python/lib/python2.4/site-packages/zenoss.extjs-3.1.0.1-py2.4.egg/zenoss/extjs/src/adapters/ext/ext-base-debug.js"
   "/Users/joseph/zenoss/python/lib/python2.4/site-packages/zenoss.extjs-3.1.0.1-py2.4.egg/zenoss/extjs/src/ext-all-debug.js"
   ) "All of the libaries that jscomint need to eval at load up time")

(defun js-comint-load-libraries ()
  "iterate through each JS library i need open and loads it in the running comint shell
This assumes the shell to be open and is called *js*"
  (interactive)
  (switch-to-buffer "*js*")
  (mapcar
   (lambda (library)
     (insert (concat "load(\"" library "\");" ))
     (comint-send-input)) js-comint-libraries))

