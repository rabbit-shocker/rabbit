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
" # image
 # src = %s
%s
%s
\n")

(defvar rabbit-slide-template
"= %s
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
  "Emacs major mode for rabbit."
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

;;; insert functions

(defun rabbit-insert-title-template (rabbit-title)
  "insert a title template."
  (interactive "spresentation's title:")
  (save-excursion (insert (format rabbit-title-template
				  rabbit-title
				  rabbit-author
				  rabbit-institution
				  rabbit-theme)))
  (forward-line 9))

(defun rabbit-insert-image-template (file cap)
  "insert a image template."
  (interactive "fimage file: \nscaption:")
  (let ((size (rabbit-read-size)))
    (rabbit-print-image-template file cap size)
    (rabbit-move-after-insert-image size)))

(defun rabbit-insert-image-template-default (file cap)
  "insert a image template with default way to specify size."
  (interactive "fimage file: \nscaption:")
  (rabbit-print-image-template file cap rabbit-specify-imagesize-default)
  (rabbit-move-after-insert-image rabbit-specify-imagesize-default))


(defun rabbit-insert-slide (rabbit-slide-title)
  "insert a slide."
  (interactive "sslide title:")
  (save-excursion (insert (format rabbit-slide-template
				  rabbit-slide-title)))
  (forward-line 2))

;;; move functions

(defun rabbit-next-slide ()
  "move to next slide."
  (interactive)
  (forward-line 1)
  (re-search-forward "^= " nil t)
  (goto-char (match-beginning 0)))

(defun rabbit-previous-slide ()
  "move to previous slide."
  (interactive)
  (re-search-backward "^= " nil t)
  (goto-char (match-beginning 0)))

;;; private

(defun rabbit-setup-keys ()
  "define default key bindings."
  (define-key rabbit-mode-map "\C-c\C-r" 'rabbit-run-rabbit)
  (define-key rabbit-mode-map "\C-c\C-t" 'rabbit-insert-title-template)
  (define-key rabbit-mode-map "\C-c\C-i" 'rabbit-insert-image-template-default)
  (define-key rabbit-mode-map "\C-ci" 'rabbit-insert-image-template)
  (define-key rabbit-mode-map "\C-c\C-s" 'rabbit-insert-slide)
  (define-key rabbit-mode-map "\M-n" 'rabbit-next-slide)
  (define-key rabbit-mode-map "\M-p" 'rabbit-previous-slide))
  
(defun rabbit-buffer-filename ()
  "return file name relating current buffer."
  (or (buffer-file-name)
      (error "This buffer is empty buffer.")))

(defun rabbit-sentinel (proc state)
  (set-buffer (process-buffer proc))
  (setq rabbit-running nil))

(defun rabbit-output-buffer ()
  "return buffer for rabbit output."
  (let* ((bufname (concat "*Rabbit<"
			  (file-relative-name (rabbit-buffer-filename))
			  ">*"))
	 (buf (get-buffer-create bufname)))
    buf))

(defun rabbit-read-size ()
  "input function of way to specicfy size."
  (completing-read "way to specicfy size:"
		   rabbit-specify-imagesize-list))

(defun rabbit-print-image-template (filename caption size)
  "insert a image-template."
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
  "return strings that specify image size."
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
  "move to next location after insert image."
  (cond ((string-equal size "")
	 (forward-line 4))
	((string-equal size "size")
	 (forward-line 6))
	(t
	 (forward-line 5))))
  
(provide 'rabbit-mode)
