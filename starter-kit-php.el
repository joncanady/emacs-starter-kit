;;; starter-kit-php.el --- Some helpful PHP code
;;
;; Part of the joncanady fork of Emacs Starter Kit

(require 'php-mode)

;; for Zend Views with the .phtml extension
(add-to-list 'auto-mode-alist '("\\.phtml" . php-mode))

(provide 'starter-kit-php)
;; starter-kit-php.el ends here
