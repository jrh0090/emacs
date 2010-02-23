;; Place to load and configure all non-major libraries i use for emacs
(require 'twit)

;; org mode short cuts
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; log the time when done
(setq org-log-done t)

;; ido makes directories look better
(require 'ido)
(ido-mode t)

;; yasnippet
(add-to-list 'load-path "~/emacs/plugins/yasnippet/")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/emacs/plugins/yasnippet/snippets")


