;;; starter-kit-misc.el --- Things that don't fit anywhere else
;;
;; Part of the Emacs Starter Kit

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(setq font-lock-maximum-decoration t
      echo-keystrokes 0.1
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point t
      require-final-newline t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat dotfiles-dir "oddmuse")
      xterm-mouse-mode t
      save-place-file (concat dotfiles-dir "places"))

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Set this to whatever browser you use
;; (setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-browser-function 'browse-default-macosx-browser)
;; (setq browse-url-browser-function 'browse-default-windows-browser)
;; (setq browse-url-browser-function 'browse-default-kde)
;; (setq browse-url-browser-function 'browse-default-epiphany)
;; (setq browse-url-browser-function 'browse-default-w3m)
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "~/src/conkeror/conkeror")

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

;; Hippie expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; nxhtml stuff
(setq mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil)

;; Associate modes with file extensions

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.phtml" . php-mode))

;; hi-line
(global-hl-line-mode 1)

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
    (add-to-list 'grep-find-ignored-files "target")
    (add-to-list 'grep-find-ignored-files "*.class")))

;; Default to unified diffs
(setq diff-switches "-u")

;; Cosmetics
(setq c-default-style "bsd"
      c-basic-offset 2)

(setq tab-width 2)


;; Customizations (not user- or system-based)
; (zenburn)
;; (speedbar 1)
(setq-default truncate-lines t)

;; speedbar needs to recognize PHP files
(speedbar-add-supported-extension ".php") ; not necessarily required
(speedbar-add-supported-extension ".phtml") ; for Zend Views
(speedbar-add-supported-extension ".ini") ; config files
(add-hook 'php-mode-user-hook 'semantic-default-java-setup)
(add-hook 'php-mode-user-hook
         (lambda ()
           (setq imenu-create-index-function
                 'semantic-create-imenu-index)
           )) 


;; speedbar needs to recognize common rails files
(speedbar-add-supported-extension ".yml")
(speedbar-add-supported-extension ".yaml")
(speedbar-add-supported-extension ".rb")
(speedbar-add-supported-extension ".html")
(speedbar-add-supported-extension ".erb")

(set-face-background 'vertical-border "white")
(set-face-foreground 'vertical-border "white")

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))


;; old projects have so many wrong line-endings
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))


;; textmate minor mode is awesome
;; http://ozmm.org/posts/textmate_minor_mode.htm
;; (require 'textmate)
;; (textmate-mode)

(eval-after-load 'mumamo
  '(eval-after-load 'zenburn
     '(ignore-errors (set-face-background
                      'mumamo-background-chunk-submode "gray22"))))

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; Get around the emacswiki spam protection
(add-hook 'oddmuse-mode-hook
          (lambda ()
            (unless (string-match "question" oddmuse-post)
              (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))))


(defun comment-uncomment-line-or-region (&optional arg)
 "comments or uncomments a line according to state before. 
With key pressed, continues with next line. 
With arg copies and reinserts last line."
  (interactive "*P")
  (comment-normalize-vars)
  (let* ((arg (if arg (prefix-numeric-value arg) 0))
         (start (if (and mark-active transient-mark-mode)
                    (region-beginning)
                  (line-beginning-position)))
         (end (if (and mark-active transient-mark-mode)
                  (region-end)
                (line-end-position)))
         (line-to-comment-or-uncomment (buffer-substring-no-properties
                                        (or
                                         start (line-beginning-position))
                                        (or end
                                            (line-end-position)))))
    (cond ((eq 1 arg) ;; comment and reinsert
           (comment-or-uncomment-region start end)
           (indent-according-to-mode) 
           (end-of-line)
           (newline)
           (insert line-to-comment-or-uncomment)
           (indent-according-to-mode))
          ((< 1 arg) ;; comment as many lines are given
           (while (<= 1 (prefix-numeric-value arg))
             (comment-or-uncomment-region (line-beginning-position) 
                                          (line-end-position))
             (indent-according-to-mode) 
             (end-of-line)
 
             ;; (indent-according-to-mode)
             (setq arg (1- arg))))
          ((and start end)
           (comment-or-uncomment-region start end)
           (indent-according-to-mode)
           (if (eobp)
               (progn (newline)
                      (indent-according-to-mode))
             (progn
 
               (indent-according-to-mode))))
          (t ;; just one line
           (progn (comment-or-uncomment-region (line-beginning-position) 
                                               (line-end-position))
                  (indent-according-to-mode)
                  (if (eobp)
                      (progn (newline)
                             (indent-according-to-mode))
                    (progn
                      (indent-according-to-mode))))))))

(global-set-key (kbd "C-;") 'comment-uncomment-line-or-region)



(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))
(setq exec-path (append exec-path '("/opt/local/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/mysql/bin"))
(setq exec-path (append exec-path '("/usr/local/mysql/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/sbin"))
(setq exec-path (append exec-path '("/opt/local/sbin")))

(setq c-auto-newline t)

(provide 'starter-kit-misc)
;;; starter-kit-misc.el ends here
