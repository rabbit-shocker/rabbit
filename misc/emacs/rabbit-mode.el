;;; -*- Emacs-Lisp -*-
;;; rabbit-mode.el
;;  Emacs major mode for Rabbit
;;; Copyright (c) 2006 Atsushi TAKEDA <tkdats@kono.cis.iwate-u.ac.jp>
;;; $Date$

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
"\n # image
 # src = %s
%s
%s
\n")

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

(defvar rabbit-specify-imagesize-list
  '(("size")
    ("normalized_width")
    ("normalized_height")
    ("relative_width")
    ("relative_height")))

(defvar rabbit-specify-imagesize-default
  "relative-height")

(define-derived-mode rabbit-mode rd-mode "Rabbit"
  (setq-default rabbit-running nil)
  (make-variable-buffer-local 'rabbit-running)
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
    (set-buffer outbuf)
    (if rabbit-running
	(error "Rabbit is already running.")
      (progn
	(setq rabbit-running t)
	(start-process "Rabbit" outbuf rabbit-command filename)
	(set-process-sentinel (get-buffer-process outbuf) 'rabbit-sentinel)))))

;; insert-procedures

(defun rabbit-insert-title-template (rabbit-title)
  (interactive "spresentation's title:")
  (save-excursion (insert (format rabbit-title-template
				  rabbit-title
				  rabbit-author
				  rabbit-institution
				  rabbit-theme)))
  (forward-line 9))

(defun rabbit-insert-image-template (file cap)
  (interactive "fimage file: \nscaption:")
  (let ((size (rabbit-read-size)))
    (rabbit-print-image-template file cap size)
    (rabbit-move-after-insert-image size)))

(defun rabbit-insert-image-template-default (file cap)
  (interactive "fimage file: \nscaption:")
  (rabbit-print-image-template file cap rabbit-specify-imagesize-default)
  (rabbit-move-after-insert-image rabbit-specify-imagesize-default))


(defun rabbit-insert-slide (rabbit-slide-title)
  (interactive "sslide title:")
  (save-excursion (insert (format "\n= %s\n\n" rabbit-slide-title)))
  (forward-line 3))

;;; private

(defun rabbit-setup-keys ()
  (define-key rabbit-mode-map "\C-c\C-r" 'rabbit-run-rabbit)
  (define-key rabbit-mode-map "\C-c\C-t" 'rabbit-insert-title-template)
  (define-key rabbit-mode-map "\C-c\C-i" 'rabbit-insert-image-template-default)
  (define-key rabbit-mode-map "\C-ci" 'rabbit-insert-image-template)
  (define-key rabbit-mode-map "\C-c\C-s" 'rabbit-insert-slide))
  
(defun rabbit-buffer-filename ()
  (or (buffer-file-name)
      (error "This buffer is empty buffer.")))

(defun rabbit-sentinel (proc state)
  (set-buffer (process-buffer proc))
  (setq rabbit-running nil))

(defun rabbit-output-buffer ()
  (let* ((bufname (concat "*Rabbit<"
			  (file-relative-name (rabbit-buffer-filename))
			  ">*"))
	 (buf (get-buffer-create bufname)))
    buf))

(defun rabbit-read-size ()
  (completing-read "type of size specicfy:"
		   rabbit-specify-imagesize-list))

(defun rabbit-print-image-template (filename caption size)
  (let ((file (file-relative-name filename))
	(cap (if (string-equal caption "")
		 "# # caption ="
	       (format " # caption = %s" caption)))
	(size-string (rabbit-read-size-value-string size)))
    (save-excursion (insert (format rabbit-image-template
				    file
				    cap
				    size-string)))))

(defun rabbit-read-size-value-string (size)
  (cond
   ((string-equal size "")
    (format ""))
   ((string-equal size "size")
    (format " # width = %s\n # height = %s"
	    (read-from-minibuffer "width:")
	    (read-from-minibuffer "height:")))
   (t
     (format " # %s = %s"
	     size
	     (read-from-minibuffer (format "%s:" size))))))

(defun rabbit-move-after-insert-image (size)
  (cond ((string-equal size "")
	 (forward-line 5))
	((string-equal size "size")
	 (forward-line 7))
	(t
	 (forward-line 6))))
  
(provide 'rabbit-mode)
