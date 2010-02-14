;; a file for all of my custom emacs functions


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

(defun push-trunk ()
  "pushes all of the trunk code live"
  (interactive)
  (shell-command "~/push_trunk.sh &")
  )

(defun push-stable ()
  "pushes all of the trunk code live"
  (interactive)
  (shell-command "~/push_stable.sh &")
  )

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


(defun switch-to-zope ()
  "Switchs to my zope.output file, where i keep my plone instance"
  (interactive)
  (switch-to-buffer "zope.out")
  (goto-char (point-max)))
(global-set-key (kbd "\C-c 6") 'switch-to-zope)

(defun restart-zope ()
  "Restarts your zope server kind of in the background. It really just
jumps around the buffers really quickly. This assumes that you have your
zope instance in a shell file called zope.out "
  (interactive)
  (save-excursion
    (switch-to-zope)
    ;; stop the previous instance
    (comint-interrupt-subjob) 
    (goto-char (point-max))
    ;; new instance command
    (insert "runzope")
    ;; press Enter
    (comint-send-input)
    ;; put them back where they were
    (switch-to-buffer (other-buffer))
    (message "Restarted your zopes")))
(global-set-key "\C-x\C-r" 'restart-zope)