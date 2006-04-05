;;; -*- Emacs-Lisp -*-

;;; rabbit-mode.el
;;  Emacs major mode for Rabbit
;;; Copyright (c) 2006 武田篤志 <tkdats@kono.cis.iwate-u.ac.jp>
;;; Last update 2006/03/19 Sun

;;; Install
;;
;; (autoload 'rabbit-mode "rabbit-mode" "major mode for Rabbit" t)
;; (add-to-list 'auto-mode-alist '("\\.\\(rbt\\|rab\\)$" . rabbit-mode))

;;; 変数
;;
;; rabbit-author - 作者
;; rabbit-institution 所属
;; rabbit-theme テーマ(デフォルトはrabbit)

;;; 機能
;;
;; rabbit-run-rabbit - Rabbitを起動
;; rabbit-insert-title-template - タイトルのテンプレートの挿入
;; rabbit-insert-image-template - 画像のテンプレートの挿入
;; rabbit-insert-slide - スライドの挿入

;;; アイディア
;;
;; * 挿入はyatexのようにC-b SPC アイテム名でキーバインドをまとめる
;;   * 挿入できるアイテムを増やす
;;     * 表
;;     * 数式
;; * Emacs上でスライドの移動
;; * スライド単位の移動
;; * 一覧バッファの生成
;;   * スライド
;;   * image
;; * 異常終了のメッセージをバッファに出す

(require 'rd-mode)

(defvar rabbit-mode-hook nil
  "Hooks run when entering `rabbit-mode' major mode")
(defvar rabbit-command "rabbit")
(defvar rabbit-output-buffer nil)
(defvar rabbit-author "作者")
(defvar rabbit-institution "所属")
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

(make-variable-buffer-local 'rabbit-running)
(setq-default rabbit-running nil)

(define-derived-mode rabbit-mode rd-mode "Rabbit"
  (define-key rabbit-mode-map "\C-c\C-r" 'rabbit-run-rabbit)
  (define-key rabbit-mode-map "\C-c\C-t" 'rabbit-insert-title-template)
  (define-key rabbit-mode-map "\C-c\C-i" 'rabbit-insert-image-template)
  (define-key rabbit-mode-map "\C-c\C-s" 'rabbit-insert-slide)
  (run-hooks 'rabbit-mode-hook))

;;; interactive

(defun rabbit-run-rabbit ()
  (interactive)
  (let ((filename (rabbit-buffer-filename))
	 (outbuf (rabbit-output-buffer)))
    (if rabbit-running
	(error "Rabbitは既に起動しています．")
      (progn
	  (setq rabbit-running t)
	  (start-process "Rabbit" outbuf rabbit-command filename)
	  (set-process-sentinel (get-buffer-process outbuf) 'rabbit-sentinel)))))
      
      
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

(defun rabbit-buffer-filename ()
  (or (buffer-file-name)
      (error "このバッファはファイルではありません．")))

(defun rabbit-sentinel (proc state)
  (kill-buffer (process-buffer proc)))
;; (setq rabbit-running nil)) ; 起動フラグを書き換えて多重起動を禁止しようとしたけど何故かできなかった

;; バッファの処理
(defun rabbit-output-buffer ()
  (let* ((bufname (concat "*Rabbit<" (rabbit-buffer-filename) ">*"))
	(buf (get-buffer-create bufname)))
    (set-buffer buf)
    buf))

(provide 'rabbit-mode)
