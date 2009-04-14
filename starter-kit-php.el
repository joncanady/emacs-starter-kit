;;; starter-kit-php.el --- Some helpful PHP code
;;
;; Part of the joncanady fork of Emacs Starter Kit

(require 'php-mode)

;; for Zend Views with the .phtml extension
(add-to-list 'auto-mode-alist '("\\.phtml" . php-mode))

(require 'flymake-php)
(add-hook 'php-mode-user-hook 'flymake-php-load)

(provide 'starter-kit-php)
;; starter-kit-php.el ends here
