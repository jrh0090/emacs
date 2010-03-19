
;; local directory of plugins
(add-to-list 'load-path "~/emacs")
(add-to-list 'load-path "~/.emacs.d/")

(load "funcs.el")
(load "environment.el")
(load "libraries.el")
(load "zenoss-config.el")
(load "shortcuts.el")
(load "php-config.el")
(load "python-config.el")
(load "javascript-config.el")

;; always start with a shell
(shell)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes (quote (("left8" (ecb-directories-buffer-name 0.1925925925925926 . 0.2894736842105263) (ecb-sources-buffer-name 0.1925925925925926 . 0.23684210526315788) (ecb-methods-buffer-name 0.1925925925925926 . 0.2894736842105263) (ecb-history-buffer-name 0.1925925925925926 . 0.17105263157894737)))))
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

