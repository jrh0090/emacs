;; Place to load and configure all non-major libraries i use for emacs
(require 'twit)

;;; Org Mode 
;; org mode short cuts
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; log the time when done
(setq org-log-done t)
;; show blocked tasks 
(setq org-agenda-dim-blocked-tasks t)

;;; IDO Mode
;; ido makes directories look better
(require 'ido)
(ido-mode t)

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t)) (visit-tags-table-buffer))
    (find-file (expand-file-name
                (ido-completing-read "Project file: "
                                     (tags-table-files) nil t)))))
(global-set-key (kbd "\C-x\C-g") 'ido-find-file-in-tag-files)

;; ido for M-x
(require 'smex)
(add-hook 'after-init-hook 'smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;; YaSnippet
;; yasnippet
(add-to-list 'load-path "~/emacs/plugins/yasnippet/")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/emacs/plugins/yasnippet/snippets")

;; browse the kill ring
(require 'browse-kill-ring)

;; winring
(require 'winring)

;; cedet
(load-file "~/emacs/cedet-1.0pre7/common/cedet.el")
(global-ede-mode t)

;; ecb
(add-to-list 'load-path
             "~/emacs/ecb-snap/")
(require 'ecb)
(add-to-list 'ecb-source-path
             "~/dev/sandbox/Products")
(ecb-activate)

;; drag stuff
(load-file "~/emacs/drag-stuff.el")
(drag-stuff-mode t)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


(load-file "~/.emacs.d/auto-complete.el")
(load-file "~/.emacs.d/auto-complete-config.el")