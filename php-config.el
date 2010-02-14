;; get rid of auto indent in C mode, hurting me more than helping me now
(require 'cc-mode)
(add-to-list 'c-mode-common-hook
          (lambda () (setq c-syntactic-indentation 1)))

(add-to-list 'c-mode-common-hook
          (lambda () (setq c-auto-newline 0)))

;; newest version of php-mode. The one in packages was like 4 years old
(load "php-mode")
;; php manual location)
;; (setq php-manual-path "~/php-manual")
;;by default it is alt-tab, didn't really plan that one out
(define-key php-mode-map "\C-c\C-t" 'php-complete-function)
;;(load "smarty-mode")

;; set the c style for my curly braces and such
(setq c-default-style "k&r")

;; flymake php syntax
(defun flymake-php-init ()
  "Use php to check the syntax of the current file."
  (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
     (local (file-relative-name temp (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local "-l"))))

(add-to-list 'flymake-err-line-patterns
  '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))

(add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))
(add-hook 'php-mode-hook (lambda () (flymake-mode 1)))

;; flymake short cuts
(define-key php-mode-map '[M-S-up] 'flymake-goto-prev-error)
(define-key php-mode-map '[M-S-down] 'flymake-goto-next-error)
