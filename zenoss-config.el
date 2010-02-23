
;; associate .tpl files with html-mode
(setq auto-mode-alist (cons '("\\.tpl$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pt$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zcml$" . sgml-mode) auto-mode-alist))

;; cpy files as python (stupid plone)
(setq auto-mode-alist (cons '("\\.cpy$" . python-mode) auto-mode-alist))

;; all of this stuff automatically cleans up files and turns
;; tabs to spaces when i save
(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun progmodes-hooks ()
  "Hooks for programming modes"
  (yas/minor-mode-on)
  (add-hook 'before-save-hook 'progmodes-write-hooks))

(defun progmodes-write-hooks ()
  "Hooks which run on file write for programming modes"
  (prog1 nil
    (set-buffer-file-coding-system 'utf-8-unix)
    (untabify-buffer)
    (maybe-delete-trailing-whitespace)))

(defun delete-trailing-whitespacep ()
  "Should we delete trailing whitespace when saving this file?"
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (next-line 25))
    (let ((pos (point)))
      (goto-char (point-min))
      (and (re-search-forward (concat "@author +" user-full-name) pos t) t))))

(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace if I am the author of this file."
  (interactive)
  (and (delete-trailing-whitespacep) (delete-trailing-whitespace)))


(defun ws ()
  "Make sure there is a space after every comma"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ",\\([^[:space:]\n,]\\)" nil t) (replace-match ", \\1"))
    (goto-char (point-min))
    (while (search-forward "( " nil t) (replace-match "("))
    (goto-char (point-min))
    (while (search-forward " )" nil t) (replace-match ")"))
    (goto-char (point-min))
    (while (search-forward "if(" nil t) (replace-match "if ("))
    (goto-char (point-min))
    (while (search-forward "for(" nil t) (replace-match "for ("))
    (goto-char (point-min))
    (while (search-forward "while(" nil t) (replace-match "while ("))
    (goto-char (point-min))
    (while (search-forward "){" nil t) (replace-match ") {"))
    (goto-char (point-min))
    (while (re-search-forward ")[[:space:]]+{" nil t) (replace-match ") {"))
    (c-set-offset 'case-label '+)
    (c-set-offset 'statement-case-open '+)
    (indent-region (point-min) (point-max) nil)
    ))

;; modes i want to untabify etc
(add-hook 'php-mode-hook 'progmodes-hooks)
(add-hook 'python-mode-hook 'progmodes-hooks)
(add-hook 'js2-mode-hook 'progmodes-hooks)

;; zope functions
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

;; Trac functions
(defvar trac-public-url "http://dev.zenoss.org/trac/ticket/" "Where tickets are on the public trac")

(defun trac-browse-public-url()
  "Browse to the current word as a trac ticket on the public trac url"
  (interactive)
  (browse-url (concat trac-public-url (current-word t))))

;; SVN helper functions
