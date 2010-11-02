
;; -*- emacs-lisp -*-
;;
;;
;; name: color-theme-bespin
;; date: Mon Nov 01 2010 11:39:08 GMT-0400 (EDT)
;;
;;
;; To use this theme save this code into a
;; file named color-theme-bespin.el and place it
;; in a directory in your load-path
;;
;;    (require 'color-theme-bespin)
;;    (color-theme-bespin)
;;


(require 'color-theme)

(defun color-theme-bespin ()
  "Generated with http://color-theme-select.heroku.com/ on Mon Nov 01 2010 11:39:08 GMT-0400 (EDT)  
color-theme-bespin"
  (interactive)
  (color-theme-install
    '(color-theme-bespin
      (
       (background-color . "#28211C")
       (foreground-color . "#BAAE9E")
       (cursor-color . "#A7A7A7")
      )
      (
      )
      (modeline  ((t (:background "white" :foreground "black" :box ( :line-width 1 :style released-button )))))
      (font-lock-builtin-face  ((t (:foreground "#A6E22A"))))
      (font-lock-comment-face  ((t (:italic t :foreground "#666666" :slant italic))))
      (font-lock-constant-face  ((t (:foreground "#DDF2A4"))))
      (font-lock-doc-string-face  ((t (:foreground "#5EA6EA"))))
      (font-lock-string-face  ((t (:foreground "#54BE0D"))))
      (font-lock-function-name-face  ((t (:foreground "#937121" :slant italic))))
      (font-lock-keyword-face  ((t (:foreground "#5EA6EA"))))
      (font-lock-type-face  ((t (:underline t :foreground "#89BDFF"))))
      (font-lock-variable-name-face  ((t (:foreground "#7587A6" :weight bold))))
      (font-lock-warning-face  ((t (:bold t :foreground "#F9EE98" :weight bold))))
      (highlight-80+  ((t (:background "#F9EE98"))))
      (hl-line  ((t (:background "#1A1A1A"))))
      (region  ((t (:background "#1a1a1a"))))
      (ido-subdir  ((t (:foreground "#F1266F"))))
     )
  )
)

(provide 'color-theme-bespin)