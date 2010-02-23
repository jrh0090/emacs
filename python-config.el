;; flymake python (pyflakes)
;; stolen from http://www.plope.com/Members/chrism/flymake-mode
(when (load "flymake" t) 
  (defun flymake-pyflakes-init () 
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                       'flymake-create-temp-inplace)) 
           (local-file (file-relative-name 
                        temp-file 
                        (file-name-directory buffer-file-name)))) 
      (list "pyflakes" (list local-file)))) 
  
  (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pyflakes-init))) 

(add-hook 'find-file-hook 'flymake-find-file-hook)


;; Run Pep8 on the current file
(defun pep8-current-buffer ()
  "Runs pep8 on the current buffer and outputs the result in the other window"
  (interactive)
  (shell-command-other-window "pep8" "pep8-output"))