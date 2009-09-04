;;; -*- mode: Emacs-Lisp; indent-tabs-mode: nil -*-
;;; rabbit-mode.el
;;  Emacs major mode for Rabbit
;;; Copyright (c) 2006 - 2008 Atsushi TAKEDA <tkdats@kono.cis.iwate-u.ac.jp>
;;; Copyright (c) 2009 Kouhei Sutou <kou@cozmixng.org>
;;; $Id$

(require 'cl)
(require 'rd-mode)

(defvar rabbit-mode-hook nil
  "Hooks run when entering `rabbit-mode' major mode")
(defvar rabbit-command "rabbit")
(defvar rabbit-command-command "rabbit-command")
(defvar rabbit-output-buffer nil)
(defvar rabbit-author nil)
(defvar rabbit-institution nil)
(defvar rabbit-theme "rabbit")

(defvar rabbit-title-metadata
  '("subtitle" "content_source" "author" "institution" "theme"))

(defvar rabbit-slide-header-regexp "^=[ \t]*\\(.*\\)")

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
   '("^==+ .*$"
     0 rabbit-comment-face)
   `(,rabbit-slide-header-regexp
     0 rabbit-heading-face)
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

(defvar rabbit-block-indent-size 2)

(defvar rabbit-image-size-unit-table
  '(("relative")
    ("normalized")
    ("pixel")))

(defvar rabbit-default-image-size-unit
  "relative")

(defvar rabbit-image-size-unit-history nil)

(define-derived-mode rabbit-mode rd-mode "Rabbit"
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
    (if (get-buffer-process outbuf)
        (error "Rabbit is already running")
      (progn
	(start-process "Rabbit" outbuf rabbit-command filename)
        (if (one-window-p)
            (set-window-buffer (split-window) outbuf)
          (set-window-buffer (previous-window) outbuf))))))

;;; insert functions

(defun rabbit-insert-title-template (title)
  "insert a title template."
  (interactive "spresentation's title: ")
  (insert (concat
           (rabbit-join-without-empty-string
            `(,(concat "= " title "\n")
              ,@(rabbit-make-metadata-strings))
            "\n")
           "\n")))

(defun rabbit-insert-image-template (file)
  "insert a image template."
  (interactive "fimage file: ")
  (rabbit-insert-image-template-real file (rabbit-read-size-unit)))

(defun rabbit-insert-image-template-default (file)
  "insert a image template with default image size unit."
  (interactive "fimage file: ")
  (rabbit-insert-image-template-real file))

(defun rabbit-insert-items ()
  (interactive)
  (let ((item (read-from-minibuffer "item: ")))
    (unless (string-equal item "")
      (insert (rabbit-block-indent (concat "* " item "\n")))
      (rabbit-insert-items))))

;;; slide functions

(defun rabbit-insert-slide (slide-title)
  "insert a slide."
  (interactive "sslide title: ")
  (insert (concat "= " slide-title "\n\n")))

(defun rabbit-delete-slide ()
  "delete a current slide."
  (interactive)
  (rabbit-fancall-with-current-point 'delete-region))

(defun rabbit-copy-slide ()
  "copy a current slide."
  (interactive)
  (rabbit-fancall-with-current-point 'kill-ring-save))

(defun rabbit-duplicate-slide ()
  "copy a current slide and yank it as next slide."
  (interactive)
  (rabbit-copy-slide)
  (rabbit-next-slide)
  (yank))

;;; move functions

(defun rabbit-next-slide ()
  "move to next slide."
  (interactive)
  (rabbit-forward-slide)
;;   (forward-line 1)
;;   (goto-char )
  (recenter))

(defun rabbit-previous-slide ()
  "move to previous slide."
  (interactive)
  (rabbit-backward-slide)
;;   (goto-char )
  (recenter))

(defun rabbit-remote (&rest args)
  (shell-command-to-string
   (mapconcat 'identity (cons rabbit-command-command args) " ")))

(defun rabbit-remote-next ()
  "move to the next in Rabbit."
  (interactive)
  (rabbit-remote "--next-slide"))

(defun rabbit-remote-previous ()
  "move to the previous in Rabbit."
  (interactive)
  (rabbit-remote "--previous"))

(defun rabbit-remote-next-slide ()
  "move to the next slide in Rabbit."
  (interactive)
  (rabbit-remote "--next-slide"))

(defun rabbit-remote-previous-slide ()
  "move to the previous slide in Rabbit."
  (interactive)
  (rabbit-remote "--previous-slide"))

(defun rabbit-remote-first-slide ()
  "move to the first slide in Rabbit."
  (interactive)
  (rabbit-remote "--first-slide"))

(defun rabbit-remote-last-slide ()
  "move to the last slide in Rabbit."
  (interactive)
  (rabbit-remote "--last-slide"))

(defun rabbit-remote-current-slide-rd ()
  "get RD of the current slide in Rabbit."
  (interactive)
  (rabbit-remote "--current-slide-rd"))

(defun rabbit-remote-toggle-fullscreen ()
  "toggle fullscreen in Rabbit."
  (interactive)
  (rabbit-remote "--toggle-fullscreen"))

(defun rabbit-remote-toggle-index-mode ()
  "toggle index mode in Rabbit."
  (interactive)
  (rabbit-remote "--toggle-index-mode"))

(defun rabbit-remote-toggle-whiteout ()
  "toggle whiteout in Rabbit."
  (interactive)
  (rabbit-remote "--toggle-whiteout"))

(defun rabbit-remote-toggle-blackout ()
  "toggle blackout in Rabbit."
  (interactive)
  (rabbit-remote "--toggle-blackout"))

(defun rabbit-remote-quit ()
  "quit Rabbit."
  (interactive)
  (rabbit-remote "--quit"))

;;; private

(defun rabbit-setup-keys ()
  "define default key bindings."
  (define-key rabbit-mode-map "\C-c\C-r" 'rabbit-run-rabbit)
  (define-key rabbit-mode-map "\C-c\C-t" 'rabbit-insert-title-template)
  (define-key rabbit-mode-map "\C-c\C-i" 'rabbit-insert-image-template-default)
  (define-key rabbit-mode-map "\C-ci" 'rabbit-insert-image-template)
  (define-key rabbit-mode-map "\C-c\C-s" 'rabbit-insert-slide)
  (define-key rabbit-mode-map "\C-c\C-d" 'rabbit-delete-slide)
  (define-key rabbit-mode-map "\M-n" 'rabbit-next-slide)
  (define-key rabbit-mode-map "\M-p" 'rabbit-previous-slide))

(defun rabbit-make-metadata-strings ()
  (let ((result '(""))
        (rabbit-subtitle nil)
        (rabbit-content_source nil))
    (dolist (metadata rabbit-title-metadata (reverse result))
      (let ((valuable (intern (concat "rabbit-" metadata))))
        (if (eval valuable)
            (setq result (cons (rabbit-metadata-string-template metadata
                                                                (eval valuable))
                               result))
          (let ((value (read-from-minibuffer (concat metadata ": "))))
            (if (string-equal value "")
                ""
              (setq result (cons (rabbit-metadata-string-template metadata value)
                                 result)))))))))

(defun rabbit-metadata-string-template (metadata value)
  (concat ": " metadata "\n"
          (rabbit-block-indent " ") value "\n"))

(defun rabbit-read-property (key)
  "read `key' value from minibuf and return string as \"key = value\"
format if value is specified, otherwise return \"\"."
  (let ((value (read-from-minibuffer (concat key ": "))))
    (if (string-equal value "")
        ""
      (concat key " = " value))))

(defun rabbit-block-indent (string)
  (let ((indent ""))
    (dotimes (i rabbit-block-indent-size (concat indent string))
      (setq indent (concat indent " ")))))

(defun rabbit-read-block-property (key)
  "read `key' value from minibuf and return string as \"key = value\"
format if value is specified, otherwise return \"\"."
  (let ((property (rabbit-read-property key)))
    (if (string-equal property "")
        ""
      (rabbit-block-indent (concat "# " property)))))

(defun rabbit-buffer-filename ()
  "return file name relating current buffer."
  (or (buffer-file-name)
      (error "This buffer is empty buffer")))

(defun rabbit-output-buffer ()
  "return buffer for rabbit output."
  (let* ((bufname (concat "*Rabbit<"
			  (file-relative-name (rabbit-buffer-filename))
			  ">*"))
	 (buf (get-buffer-create bufname)))
    buf))

(defun rabbit-read-size-unit ()
  "read what unit use for image size."
  (completing-read "image size unit: "
		   rabbit-image-size-unit-table
                   nil t nil
                   'rabbit-image-size-unit-history
                   rabbit-default-image-size-unit))

(defun rabbit-filter (predicate lst)
  (let ((result '()))
    (dolist (item lst (reverse result))
      (unless (funcall predicate item)
        (setq result (cons item result))))))

(defun rabbit-not-empty-string (string)
  (and string (not (string-equal string ""))))

(defun rabbit-join-without-empty-string (strings separator)
  (let ((result "")
        (not-empty-strings (rabbit-filter
                            '(lambda (string)
                               (not (rabbit-not-empty-string string)))
                            strings)))
    (if (null not-empty-strings)
        ""
      (dolist (string (cdr not-empty-strings)
                      (concat (car not-empty-strings) result))
        (setq result (concat result separator string))))))

(defun rabbit-insert-image-template-real (filename &optional unit)
  "insert a image template."
  (insert (concat
           (rabbit-join-without-empty-string
            `(,(rabbit-block-indent "# image")
              ,(rabbit-block-indent (concat "# src = "
                                            (file-relative-name filename)))
              ,(rabbit-read-block-property "caption")
              ,@(rabbit-read-size-value
                 (or unit rabbit-default-image-size-unit)))
            "\n")
           "\n")))

(defun rabbit-read-size-value (unit)
  "return strings that specify image size."
  (let ((prefix (if (string-equal unit "pixel")
                    ""
                  (concat unit "_"))))
    (mapcar '(lambda (key)
               (rabbit-read-block-property (concat prefix key)))
            '("width" "height"))))

(defun rabbit-fancall-with-current-point (func)
  (multiple-value-bind (beg end)
      (rabbit-current-slide-point)
    (funcall func beg end)))

(defun rabbit-current-slide-point ()
  (values (save-excursion (rabbit-forward-slide)
                          (point))
          (save-excursion (end-of-line)
                          (rabbit-backward-slide)
                          (point))))

(defun rabbit-forward-slide ()
  (end-of-line)
  (or (re-search-forward rabbit-slide-header-regexp nil t)
      (end-of-buffer))
  (beginning-of-line))

(defun rabbit-backward-slide ()
  (or (re-search-backward rabbit-slide-header-regexp nil t)
      (beginning-of-buffer)))

(provide 'rabbit-mode)
