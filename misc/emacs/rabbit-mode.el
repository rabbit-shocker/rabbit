;;; -*- Emacs-Lisp -*-
;;; rabbit-mode.el
;;  Emacs major mode for Rabbit
;;; Copyright (c) 2006 Atsushi TAKEDA <tkdats@kono.cis.iwate-u.ac.jp>
;;; $Date$

;;; Install
;;
;; (autoload 'rabbit-mode "rabbit-mode" "major mode for Rabbit" t)
;; (add-to-list 'auto-mode-alist '("\\.\\(rbt\\|rab\\)$" . rabbit-mode))

;;; Variables
;;
;; rabbit-author - author of presentation
;; rabbit-institution - author's institution 
;; rabbit-theme - theme of presentation
;; rabbit-heading-face - face of slide line (= hoge) 
;; rabbit-emphasis-face - face of emphasis ((* ... *)) 
;; rabbit-verbatim-face - face of verbatim ((' ... '))
;; rabbit-term-face -  face of term ((: ... :))
;; rabbit-footnote-face - face of footnote ((- ... -))
;; rabbit-link-face - face of link ((< ... >))
;; rabbit-code-face - face of code (({ ... }))
;; rabbit-description-face - face of labeled list (:hoge)
;; rabbit-keyboard-face - face of keyboard input ((% ... %))
;; rabbit-variable-face - face of variable ((| ... |))
;; rabbit-comment-face - face of comment (# hoge)

;;; Functions
;;
;; rabbit-run-rabbit - run Rabbit
;; rabbit-insert-title-template - insert a title template
;; rabbit-insert-image-template - insert a image template
;; rabbit-insert-slide - insert a slide

(require 'rd-mode)

(defvar rabbit-mode-hook nil
  "Hooks run when entering `rabbit-mode' major mode")
(defvar rabbit-command "rabbit")
(defvar rabbit-output-buffer nil)
(defvar rabbit-author "Author")
(defvar rabbit-institution "Institution")
(defvar rabbit-theme "rabbit")
(defvar rabbit-title-template 
"= %s

: author
   %s
: institution
   %s
: theme
   %s
\n")

(defvar rabbit-image-template 
" # image
 # src = %s
 # caption = 
 # width = 100
 # height = 100
# # normalized_width = 50
# # normalized_height = 50
# # relative_width = 100
# # relative_height = 50
")

(defvar rabbit-heading-face 'font-lock-keyword-face)
(defvar rabbit-emphasis-face 'font-lock-function-name-face)
(defvar rabbit-verbatim-face 'font-lock-function-name-face)
(defvar rabbit-term-face 'font-lock-function-name-face)
(defvar rabbit-footnote-face 'font-lock-function-name-face)
(defvar rabbit-link-face 'font-lock-function-name-face)
(defvar rabbit-code-face 'font-lock-function-name-face)
(defvar rabbit-description-face 'font-lock-constant-face)
(defvar rabbit-comment-face 'font-lock-comment-face)
(defvar rabbit-keyboard-face 'font-lock-function-name-face)
(defvar rabbit-variable-face 'font-lock-function-name-face)
(defvar rabbit-font-lock-keywords
  (list
   '("^= .*$"
     0 rabbit-heading-face)
   '("^==+ .*$"
     0 rabbit-comment-face)
   '("((\\*[^*]*\\*+\\([^)*][^%]*\\*+\\)*))"    ; ((* ... *))
     0 rabbit-emphasis-face)
   '("((%[^%]*%+\\([^)%][^%]*%+\\)*))"      ; ((% ... %))
     0 rabbit-keyboard-face)
   '("((|[^|]*|+\\([^)|][^|]*|+\\)*))"      ; ((| ... |))
     0 rabbit-variable-face)
   '("(('[^']*'+\\([^)'][^']*'+\\)*))"      ; ((' ... '))
     0 rabbit-verbatim-face)
   '("((:[^:]*:+\\([^):][^:]*:+\\)*))"      ; ((: ... :))
     0 rabbit-term-face)
   '("((-[^-]*-+\\([^)-][^-]*-+\\)*))"      ; ((- ... -))
     0 rabbit-footnote-face)
   '("((<[^>]*>+\\([^)>][^>]*>+\\)*))"      ; ((< ... >))
     0 rabbit-link-face)
   '("(({[^}]*}+\\([^)}][^}]*}+\\)*))"      ; (({ ... }))
     0 rabbit-code-face)
   '("^:.*$"
     0 rd-description-face)
   '("^#.*$"
      0 rabbit-comment-face)
   ))

(define-derived-mode rabbit-mode rd-mode "Rabbit"
  (make-variable-buffer-local 'rabbit-running)
  (setq-default rabbit-running nil)
  (setq comment-start "#")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((rabbit-font-lock-keywords) t nil))
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords rabbit-font-lock-keywords)
  (rabbit-setup-keys)
  (run-hooks 'rabbit-mode-hook))

;;; interactive

(defun rabbit-run-rabbit ()
  (interactive)
  (let ((filename (rabbit-buffer-filename))
	 (outbuf (rabbit-output-buffer)))
    (if rabbit-running
	(error "Rabbit is already running.")
      (progn
	  (setq rabbit-running t)
	  (start-process "Rabbit" outbuf rabbit-command filename))
      (set-process-sentinel (get-buffer-process outbuf) 'rabbit-sentinel))))

;; insert-procedures

(defun rabbit-insert-title-template (rabbit-title)
  (interactive "spresen title:")
  (save-excursion (insert (format rabbit-title-template
				  rabbit-title
				  rabbit-author
				  rabbit-institution
				  rabbit-theme)))
  (forward-line 9))

(defun rabbit-insert-image-template (rabbit-image-title)
  (interactive "fimage file:")
  (save-excursion (insert (format rabbit-image-template
				  rabbit-image-title)))
  (forward-line)
  (forward-char 9))

(defun rabbit-insert-slide (rabbit-slide-title)
  (interactive "sslide title:")
  (save-excursion (insert (format "\n= %s\n\n" rabbit-slide-title)))
  (forward-line 3))

;;; private

(defun rabbit-setup-keys ()
  (define-key rabbit-mode-map "\C-c\C-r" 'rabbit-run-rabbit)
  (define-key rabbit-mode-map "\C-c\C-t" 'rabbit-insert-title-template)
  (define-key rabbit-mode-map "\C-c\C-i" 'rabbit-insert-image-template)
  (define-key rabbit-mode-map "\C-c\C-s" 'rabbit-insert-slide))
  
(defun rabbit-buffer-filename ()
  (or (buffer-file-name)
      (error "This buffer is empty buffer.")))

(defun rabbit-sentinel (proc state)
  (kill-buffer (process-buffer proc)))

(defun rabbit-output-buffer ()
  (let* ((bufname (concat "*Rabbit<" (rabbit-buffer-filename) ">*"))
	(buf (get-buffer-create bufname)))
    (set-buffer buf)
    buf))

(provide 'rabbit-mode)
