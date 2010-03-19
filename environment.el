;;;; All of my environmental variables and config settings go here
;;;;
;;;;

;; load up my path from the system (todo figure out why this doesnt do
;; this automatically)
(setenv "PATH"
        "/opt/local/bin:/opt/local/sbin:/opt/subversion/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:/Users/joseph/zenoss/zenoss/bin:/Xcode3.1.4/usr/bin"
        )

(setenv "ZENHOME"
        "/Users/joseph/zenoss/zenoss"
        )

(setenv "PYTHONPATH"
        "/Users/joseph/zenoss/zenoss/"
        )

;; to have my .profile variables in emacs, not sure why this is necessary
(shell-command "source ~/.profile")

;; always show matching parenthesis
(show-paren-mode t)

;; set new offsets to 4
(setq-default c-basic-offset 4)
(setq tab-width 4)

;; supposedly this makes all tabs spaces (i think it lies)
(setq indent-tabs-mode nil)
(setq default-tab-width 4)

;; set the indentation in xml and html modes
(setq sgml-basic-offset 4)

;; only for x windows
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; make the prompt read only in shell
(setq comint-prompt-read-only t)

;; high light current line
(global-hl-line-mode 0)

;; prevent back ups from being created
(setq make-backup-files nil)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; make the default mode text mode with flyspell
(setq default-major-mode 'text-mode)
;; commented out until flyspell starts working for me
;; (add-hook 'text-mode-hook 'flyspell-mode)

(defadvice ispell-command-loop (before ispell-reverse-miss-list activate)
  "reverse the first argument to ispell-command-loop"
  (ad-set-arg 0 (reverse (ad-get-arg 0))))

;; do not beep when you mess up
(setq visible-bell t)

;; remove the gui elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; highlights your marked region
(transient-mark-mode 1)

;; always highlight syntax
(global-font-lock-mode 1)

;; will always need this
(require 'flymake)

;; when you have two open buffers with the same name, make it
;; not retarded ie. Display.php <2>
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; keep all my backups in one place
(push '("." . "~/.emacs-backups") backup-directory-alist)

;; no spash screen on start up
(setq inhibit-splash-screen t)

;; use y or n instead of yes or no and then pressing return
(fset 'yes-or-no-p 'y-or-n-p)

;; always load the which-func mode (only works for python)
(which-func-mode t)

;; set the color theme
(require 'color-theme)
(color-theme-billw)

;; font
;;(set-default-font "-apple-Didot-medium-normal-normal-*-*-*-*-*-p-0-iso10646-1")

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; remember the last place in your file
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)

;; aspell
(setq-default ispell-program-name "aspell")

;; easier to navigate between frames
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))

;; mac specific settings

(if (eq system-type 'darwin)
    (progn
      ;; fixing meta for mac (makes command Meta)
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)
      ;; now delete forward deletes a char
      (global-set-key (kbd "<kp-delete>") 'delete-char)
      ;; fullscreen on mac
      (defun mac-maximize-frame () 
        (interactive)
        (set-frame-position (selected-frame) 0 0)
        (set-frame-size (selected-frame) 1000 1000))
      (mac-maximize-frame)))


