;;;; All of my environmental variables and config settings go here
;;;;
;;;;

;; load up my path from the system (todo figure out why this doesnt do
;; this automatically)
(setenv "PATH"
        "/Users/joseph/zenoss/bin:/opt/local/bin:/opt/local/sbin:/opt/subversion/bin:/opt/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:/Xcode3.1.4/usr/bin:/usr/local/mysql/bin/"
        )

(setenv "ZENHOME"
        "/Users/joseph/zenoss"
        )

(setenv "PYTHONPATH"
        "/Users/joseph/zenoss/lib/python"
        )
(setenv "PS1" "\\u:\\w$ ")

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
(add-hook 'text-mode-hook 'flyspell-mode)

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
(setq font-lock-maximum-decoration t)

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
(load-file "~/emacs/color-theme-blackboard.el")
(load-file "~/emacs/zenburn.el")
(color-theme-zenburn)

;; i like blinking cursors
(blink-cursor-mode t)

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; remember the last place in your file
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)

;; smart tab key (complete/indent)
(setq tab-always-indent 'complete)

;; aspell
;;(setq-default ispell-program-name "aspell")

;; easier to navigate between frames
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))

;; use ibuffer to list buffer contents
(global-set-key (kbd "\C-x\C-b") 'ibuffer)

;; mac specific settings
(if (eq system-type 'darwin)
    (progn
      ;; mac only font
      ;;(set-default-font "-apple-Consolas-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")
      (set-default-font "-apple-Menlo-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1")
      ;; fixing meta for mac (makes command Meta)
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)
      
      ;; now delete forward deletes a char
      (global-set-key (kbd "<kp-delete>") 'delete-char)
      
      ;; end by default goes to the end of the buffer
      (global-set-key (kbd "<end>") 'move-end-of-line)
      (global-set-key (kbd "<home>") 'back-to-indentation)
      
      ;; control Itunes with the f6 key
      (load-file "~/emacs/osx-osascript.el")
      (load-file "~/emacs/itunes.el")
      
      ;; fullscreen on mac
      (defun mac-maximize-frame () 
        (interactive)
        (set-frame-position (selected-frame) 0 0)
        (set-frame-size (selected-frame) 1000 1000))
      (mac-maximize-frame)))

;; ubuntu (vmware) specific settings
(if (eq system-type 'gnu/linux)
    (progn
      (set-default-font "-bitstream-Bitstream Charter-bold-normal-normal-*-16-*-*-*-*-0-iso10646-1")
      ))

;; never use the message box (evil!)
(defalias 'message-box 'message)

;; used so i could compile cedet
(setq max-lisp-eval-depth 1600)

;; swap windows
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

(global-set-key (kbd "C-c s") 'swap-windows)


;; display time mode (shows time and load in the menu bar)
(display-time-mode 1)