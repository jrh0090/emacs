;; a file for all of my custom emacs utility functions


;; copy line instead of kill-undo
(defun copy-line (&optional arg)
  "Do a kill-line but copy rather than kill. This function directly calls
kill-line, so see documentation of kill-line for how to use it including prefix
argument and relevant variables. This function works by temporarily making the
buffer read-only, so I suggest setting kill-read-only-ok to t."
  (interactive "P")
  (toggle-read-only 1)
  (kill-line arg)
  (toggle-read-only 0))

(setq-default kill-read-only-ok t)
(global-set-key "\M-k" 'copy-line)

(defun duplicate-line()
  "adds a copy of the current line on the line below"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(global-set-key (kbd "\C-c j") 'duplicate-line)


(defun cdc ()
  "If we are in trunk it opens the stable version"
  (interactive)
  (let*
      ((current-buffer-file (buffer-file-name (current-buffer)))
       (file-name (if (string-match "/trunk/" current-buffer-file)
                      (replace-regexp-in-string "/trunk/" "/stable/" current-buffer-file)
                    (replace-regexp-in-string "/stable/" "/trunk/" current-buffer-file))))
    ;; let* body
    (if (file-exists-p file-name)
        (find-file file-name)
      (message "file: %s does not exist" file-name))))


(defun copy-to-stable ()
  "Copies the current buffer to stable, or in stable copies it to trunk"
  (interactive)
    (save-excursion
      (mark-whole-buffer)
      (kill-ring-save (point-min) (point-max)) ;; save the whole buffer in the kill ring
      (cdc)
      (mark-whole-buffer)
      (kill-region (point-min) (point-max))
      (yank 2)))

(defun open-template ()
  "If we are the tpl file opens up the php and if in the php will open up the tpl"
  (interactive)
  (let*
      ((current-buffer-file (buffer-file-name (current-buffer)))
       (file-name (if (string-match ".php" current-buffer-file)
                      (replace-regexp-in-string ".php" ".tpl" current-buffer-file)
                    (replace-regexp-in-string ".tpl" ".php" current-buffer-file))))
    ;; let* body
    (if (file-exists-p file-name)
        (find-file file-name)
      (message "file: %s does not exist" file-name))))

(defun file-name ()
  (interactive)
  (message (buffer-file-name)))

(defun shell-command-other-window (command window-name)
  "Runs the command passed in and outputs it in the other window named by window name
This always runs on the current buffer"
  (save-excursion
    (progn
      (when (buffer-file-name)  
        (shell-command (concat command " " (buffer-file-name)) window-name)
        (switch-to-buffer-other-window window-name)
        (other-window 1)))))

(defun inc-dec-number (func)
  (let ((num (string-to-number (current-word))))
    (if num
        (save-excursion
          (forward-word 1)
          (backward-kill-word 1)
          (insert (number-to-string
                   (if (string= func "+") (+ 1 num)
                     (- num 1))))))))

(defun increment-number()
  (interactive)
  (inc-dec-number "+"))
(global-set-key "\C-c=" 'increment-number)

(defun decrement-number()
  (interactive)
  (inc-dec-number "-"))
(global-set-key "\C-c-" 'decrement-number)

;; copy word
(defun copy-word (&optional arg)
  "Adds the next word to the kill ring, the same as mark-word followed by kill-ring-save"
  (interactive "P")
  (save-excursion
    (toggle-read-only 1)
    (kill-word arg)
    (toggle-read-only 0)))
(global-set-key "\M-r" 'copy-word)

(defun opposite-case()
  "If the current letter is uppercase, this makes it lower case, otherwise it makes it uppercase"
  (interactive)
  (let ((char  (char-after)))
    (save-excursion
      ;; remove current character
      (delete-char 1)
      ;; place the opposite one
      (insert   (if (> char 90)
                    (- char 32)
                  (+ char 32))))))
;; was formerly "insert space to tab"
(global-set-key "\M-i" 'opposite-case)


           