; -*- Mode: Lisp; Package: editor -*-

#|
= rabbit-mode - rabbit のフロントエンド

* Author: MIYAMUKO Katsuyuki ((<URL:mailto:miyamuko (at) mtb.biglobe.ne.jp>))
* URL: ((<URL:http://miyamuko.s56.xrea.com/xyzzy/rabbit-mode.html>))
* Version: 0.2


== 説明

Ruby で書かれたプレゼンツールの
((<"rabbit"|URL:http://www.cozmixng.org/~rwiki/?cmd=view;name=Rabbit%3A%3AREADME.ja>))
を少し便利に使えるようにするフロントエンドです。

rd ファイルを編集しながら xyzzy から rabbit を起動できます。

== できること

* rabbit をフルスクリーンモードで起動できる。
* rabbit を起動できる。
* タイトルページのテンプレートを挿入できる。

* ファイル名がなくても実行できる
* スライド単位での編集
  * スライド一覧を表示して jump (list-function)
  * スライド数とかを表示しつつスライド単位での移動 (C-M-n, C-M-p)
  * 非表示スライドをトグル
* 特定範囲のプレビュー
  * 範囲
    * 現在のスライドのみ
    * リージョン内のスライドのみ
    * セレクション内のスライドのみ
  * ただしタイトルページは含まれる。
    * スライド数指定で起動とかできないかな?

== やらない (できない) こと

* テーマ作成用モード

=== これからやるかもしれないこと

* サブタイトルとかを interactive に入力
* save-as-image
* テーマ名の補完
* tdiary theme から rabbit theme に変換できるとかっこいい。
  * 背景画像とかは指定できない(?)
    * できるらしい。
      * ・・・ってこれは lisp じゃなくて ruby で実装して rabbit 本体に取り込んでもらったほうがよさげ。

* rd 書き支援
  * rd 支援は ruby-doc-minor-mode に任せようと思ったけどやろうかな。
  * rd に特化した rd-mode をいい加減にちゃんとつくる。
  * 箇条書き支援
* 既存 rd 文書を rabbit 化 (rabbitalize ?)
  * 箇条書きのトップレベルをスライドのタイトルにする
    * この変換を rabbit 実行時にやる。
      * 箇条書きを変換して{プレビュー,実行}
        * 箇条書きでひたすら書きたい場合に便利。(たとえばこの todo リスト)
  * 見出しレベル2 (==) をスライドのタイトルに
* プレビューサイズ設定
* テンプレートを指定して新しいスライドの作成 (C-m)
* テンプレートの充実
  * 普通のスライド
  * mimeTex
  * image
    * keep_scale、relative_width などの補完
  * 最終スライド
  * テンプレートのテキストファイル化
    * テキストファイルで作ってどこかに置くとメニューに出てくる。
* Sigma: や sum: の実体参照（Rabbit独自拡張）の補完
* 「ここをクリックして xxx を追加」の上でクリックするとなにか起きる。
* 目的別スライドショー
  * 必要なスライド（名 or 番号) を定義して (どうやって? 特殊なコメント?) そこだけスライドショー
* xyzzy を http サーバにして RWiki/Hiki のようにページソースの提供
  * 以下のようなケースで役に立ちそう。
    * rwiki を簡単に立てられない
    * 発表マシンに rabbit が入っている
    * スライド作成マシンと発表マシンが違う (作成: Win, 発表: Linux とか)
    ・・・必要ないか。
  * RWiki 互換にする場合 soap.l も作らないとだめそう
* rabbit-mode のときのみメニュー表示
  * rd-mode をマイナーモードにして rabbit-mode をメジャーモード
  * rd-mode をメジャーモードにして rabbit-mode はモード継承
* xyzzy から rabbit の操作
  * rabbit を FindWindow して SendMessage する(?)
  * 追従モード
    * スライド単位で移動した場合は自動的に SendKey する
    * ページ数が 2 桁ある場合は無理 (?)

* =begin, =end の削除
* zphoto などの外部ツールとの連携

* rabrick
* rabbit-theme-browser

== インストール

((<NetInstaller|URL:http://www7a.biglobe.ne.jp/~hat/xyzzy/ni.html>)) でインストールした場合は
3 以降で OK です。

(1) アーカイブをダウンロードします。

    ((<URL:http://miyamuko.s56.xrea.com/xyzzy/archives/rabbit-mode.zip>))

(2) アーカイブを展開して、$XYZZY/site-lisp 配下にファイルをコピーします。

(3) ~/.xyzzy または $XYZZY/site-lisp/siteinit.l に以下のコードを追加します。

        ;; rabbit-mode
        (require "rabbit-mode")
        (pushnew '("\\.rd$" . rabbit-mode) *auto-mode-alist* :test 'equal)

(4) 設定を反映させるには xyzzy を再起動してください。

    ※siteinit.l に記述した場合には再ダンプが必要です。

(5) ruby.exe にパスを通していない場合は ((<*rabbit-command*>)) を設定します。

(6) rd ファイルを開きメニューから rabbit を起動してください。


== リファレンス

=== キー割り当て

: ((%F5%))

    スライドショーを開始します。

: ((%C-c n%))

    カーソル下のスライドをナローイングします。

: ((%C-c h%))

    カーソル下のスライドの表示・非表示をトグルします。

: ((%C-c c%))

    カーソル下のスライドをコピーして挿入します。

: ((%C-c C-c%))

    スライド一覧から選択したスライドをコピーして挿入します。

: ((%C-M-n%))

    次のスライドに移動します。

: ((%C-M-p%))

    前のスライドに移動します。

: ((%C-M-k%))

    現在のスライドを kill します。

: ((%C-M-Up%))

    現在のスライドをひとつ上に移動します。

: ((%C-M-Down%))

    現在のスライドをひとつ下に移動します。

: ((%M-NUL%))

    現在のスライドをマークします。

=== 変数

--- *rabbit-command*
    rabbit を実行するコマンドを指定します。デフォルトは (({ruby -S rabbit})) です。

    ruby.exe にパスを通していない場合やオプションを指定したい場合は
    ここで設定します。

    例:
        (setf *rabbit-command* "C:/ruby184/bin/ruby.exe -S rabbit")

    パス名に空白文字が入っているとうまくいかないと思います。

--- *rabbit-author*
--- *rabbit-institution*
    rabbit-insert-title-template でタイトルページを挿入するときの
    作者 (author) と 所属 (institution) を指定します。


== 関連するかもしれないページ

: rabbit
    ((<URL:http://www.cozmixng.org/~rwiki/?cmd=view;name=Rabbit%3A%3AREADME.ja>))

: Ruby-GNOME2 Project Website
    ((<URL:http://ruby-gnome2.sourceforge.jp/ja/index.html>))

: RAA - rabbit
    ((<URL:http://raa.ruby-lang.org/project/rabbit/>))

: RAA - ruby-gnome2
    ((<URL:http://raa.ruby-lang.org/project/ruby-gnome2/>))


== ライセンス

rabbit-mode は修正 BSD ライセンスに基づいて利用可能です。

  Copyright (C) 2004-2006 MIYAMUKO Katsuyuki. All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1 Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer as the first lines
    of this file unmodified.

  2 Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.
|#

(provide "rabbit-mode")
(in-package "editor")

(export '(rabbit-slide-show
          rabbit-preview
          rabbit-preview-current-slide
          rabbit-preview-region
          rabbit-preview-selection
          rabbit-preview-begin-end
          rabbit-save-as-image
          rabbit-insert-title-template
          rabbit-toggle-hide-slide
          rabbit-hide-slide-p
          rabbit-narrow-to-slide
          rabbit-move-up-slide
          rabbit-move-down-slide
          rabbit-copy-slide
          rabbit-copy-current-slide
          rabbit-kill-slide
          rabbit-forward-slide
          rabbit-backward-slide
          rabbit-mode

          *rabbit-command*
          *rabbit-author*
          *rabbit-institution*
          *rabbit-menu*
          *rabbit-mode-syntax-table*
          *rabbit-mode-map*
          *rabbit-mode-hook*))

(defvar *rabbit-command* "ruby -S rabbit")

;; コメントアウトされた非表示スライドも考慮
(defvar *rabbit-slide-header-regexp* "^#*=[ \t]*\\(.*\\)")

(defvar *rabbit-author* "作者")
(defvar *rabbit-institution* "所属")
(defvar *rabbit-title-template* (format nil
"= ここをクリックしてタイトルを入力

: author
    ~A
: institution
    ~A
" *rabbit-author* *rabbit-institution*))

(defvar *rabbit-menu* nil)
(defvar *rabbit-menu-name* "&Rabbit")
(defvar *rabbit-menu-position* 'ed::help)

(defvar *rabbit-mode-hook* nil)
(defvar *rabbit-comment-column* 0)

(defvar *rabbit-mode-syntax-table* nil)
(unless *rabbit-mode-syntax-table*
  (setf *rabbit-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
        (set-syntax-punctuation *rabbit-mode-syntax-table* c))))
  (set-syntax-start-comment *rabbit-mode-syntax-table* #\#)
  (set-syntax-end-comment *rabbit-mode-syntax-table* #\LFD)
  )

(defvar *rabbit-mode-map* nil)
(unless *rabbit-mode-map*
  (setf *rabbit-mode-map* (make-sparse-keymap))
  (define-key *rabbit-mode-map* '#\F5 'rabbit-slide-show)
  (define-key *rabbit-mode-map* '(#\C-c #\n) 'rabbit-narrow-to-slide)
  (define-key *rabbit-mode-map* '(#\C-c #\h) 'rabbit-toggle-hide-slide)
  (define-key *rabbit-mode-map* '(#\C-c #\c) 'rabbit-copy-slide)
  (define-key *rabbit-mode-map* '(#\C-c #\C-c) 'rabbit-copy-current-slide)
  (define-key *rabbit-mode-map* #\C-M-n 'rabbit-forward-slide)
  (define-key *rabbit-mode-map* #\C-M-p 'rabbit-backward-slide)
  (define-key *rabbit-mode-map* #\C-M-k 'rabbit-kill-slide)
  (define-key *rabbit-mode-map* #\C-M-Up 'rabbit-move-up-slide)
  (define-key *rabbit-mode-map* #\C-M-Down 'rabbit-move-down-slide)
  (define-key *rabbit-mode-map* #\M-NUL 'rabbit-mark-slide)
  )

;;
;; RD 色づけ
;;

(defvar *rabbit-regexp-keyword-list* nil)
(setf *rabbit-regexp-keyword-list*
  (compile-regexp-keyword-list
   `(
     ("^\\(=\\(?:begin\\|end\\)\\)" t ((1 . (:color 7 4))))
     ("^=\\{1,4\\} +\\([^ \t\r\f\n].*\\)$" t ((1 . (:keyword 0))))
     ("^\\+\\{1,2\\} +\\([^ \t\r\f\n].*\\)$" t ((1 . (:keyword 0))))
     ("^ *\\(:\\|---\\) *\\([^ \t\r\f\n].*\\)$" t ((2 . (:color 0 0 :underline))))
     ("^ *\\(\\*\\)" t ((1 . (:keyword 0))))
     ("^ *\\(([0-9]+)\\)" t ((1 . (:keyword 0))))
     ("(([-*{|%:<'].*?[-*}|%:>']))" t ((0 . (:keyword 1))))
     )))

;;
;; menu
;;

(defun rabbit-executable-p ()
  (interactive)
  (if (rabbit-buffer-filename)
      (< 0 (file-length (rabbit-buffer-filename)))
    (< 0 (buffer-size))))

(unless *rabbit-menu*
  (setf *rabbit-menu* (copy-menu-items
                       (define-popup-menu
                         (:item nil "スライドショー(&W)" 'rabbit-slide-show
                          (lambda () (unless (rabbit-executable-p) :disable)))
                         (:item nil "プレビュー(&V)" 'rabbit-preview
                          (lambda () (unless (rabbit-executable-p) :disable)))
                         (:popup nil "プレビュー"
                          (:item nil "現在のスライドをプレビュー(&C)" 'rabbit-preview-current-slide
                           (lambda () (unless (rabbit-executable-p) :disable)))
                          (:item nil "リージョンをプレビュー(&R)" 'rabbit-preview-region
                           (lambda () (unless (rabbit-executable-p) :disable)))
                          (:item nil "選択領域をプレビュー(&S)" 'rabbit-preview-selection
                           (lambda () (unless (rabbit-executable-p) :disable)))
                          (:item nil "=beginから=endをプレビュー(&E)" 'rabbit-preview-begin-end
                           (lambda () (unless (rabbit-executable-p) :disable))))
                         :sep
                         (:item nil "画像で保存する(&S)" 'rabbit-save-as-image
                          (lambda () :disable))
                         :sep
                         (:item nil "次のスライド(&N)" 'rabbit-forward-slide)
                         (:item nil "前のスライド(&P)" 'rabbit-backward-slide)
                         (:item nil "スライド一覧(&D)" 'list-function)
                         :sep
                         (:item nil "スライドを複製..." 'rabbit-copy-slide)
                         (:item nil "現在のスライドを複製" 'rabbit-copy-current-slide)
                         (:item nil "スライドを削除" 'rabbit-kill-slide)
                         (:item nil "スライドをマーク" 'rabbit-mark-slide)
                         :sep
                         (:item nil "非表示スライドに設定(&H)" 'rabbit-toggle-hide-slide
                          (lambda () (when (rabbit-hide-slide-p) :check)))
                         :sep
                         (:popup nil "テンプレート(&I)"
                          (:item nil "タイトルページ(&T)" 'rabbit-insert-title-template))
                         )
                       (create-menu 'rabbit))))

(defun insert-rabbit-menu (&key (menu (current-menu))
                             (position *rabbit-menu-position*)
                             (menu-name *rabbit-menu-name*))
  (when (menup menu)
    (setq menu (copy-menu-items menu (create-menu 'rabbit)))
    (delete-rabbit-menu menu)
    (cond
     ((and (numberp position) (integerp position))
      (insert-popup-menu menu position *rabbit-menu* menu-name))
     ((and (symbolp position) (get-menu-position menu position))
      (insert-popup-menu menu (get-menu-position menu position)
                         *rabbit-menu* menu-name))
     (t
      (add-popup-menu menu *rabbit-menu* menu-name)))
    (use-local-menu menu)))

(defun delete-rabbit-menu (&optional (menu (current-menu)))
  (let (ret)
    (if (menup menu)
        (while (delete-menu menu 'ed::rabbit)
          (setq ret t)))
    ret))

(defun insert-rabbit-menu-all-buffers ()
  (save-excursion
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (eq buffer-mode 'rabbit-mode)
        (insert-rabbit-menu)))))

(unless *app-menu*
  (add-hook '*post-startup-hook* 'insert-rabbit-menu-all-buffers))

;;
;; rabbit の実行
;;

(defun rabbit-slide-show (&optional theme)
  (interactive)
  (rabbit-execute-rabbit (rabbit-buffer-filename) (buffer-fileio-encoding) theme
                         "--full-screen"))

(defun rabbit-preview (&optional theme)
  (interactive)
  (rabbit-execute-rabbit (rabbit-buffer-filename) (buffer-fileio-encoding) theme))

(defun rabbit-preview-current-slide (&optional theme)
  (interactive)
  (rabbit-funcall-with-current-point 'rabbit-preview-region))

(defun rabbit-preview-selection (&optional theme)
  (interactive)
  (selection-start-end (beg end)
    (rabbit-preview-region beg end)))

;; =begin
;; ここをプレビュー
;; =end
;; こことかは無視
;; =begin
;; ここも一緒に
;; =end
(defun rabbit-preview-begin-end (&optional theme)
  (interactive)
  (let (slides)
    (save-excursion
      (goto-char (point-min))
      (while (scan-buffer "^=begin\n" :regexp t :tail t)
        (push (buffer-substring (point) (progn
                                          (or (scan-buffer "^=end\n" :regexp t) (end-of-buffer))
                                          (point)))
              slides)))
    (unless slides
      (plain-error "スライドがないです。"))
    (rabbit-execute-rabbit-argf (format nil "~{~A~%~}" (reverse slides))
                                (buffer-fileio-encoding) theme)))

(defun rabbit-preview-region (beg end &optional theme)
  (interactive "r")
  (if (< end beg) (rotatef beg end))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (rabbit-forward-slide nil)
      (setf beg (point))))
  (unless (< beg end)
    (plain-error "スライドがないです。"))
  (let ((slide) (title ""))
    (unless (= beg (rabbit-title-slide-point))
      (setf title (rabbit-title-slide)))
    (setf slide (concat title (buffer-substring beg end)))
    (rabbit-execute-rabbit-argf slide (buffer-fileio-encoding) theme)))

(defun rabbit-save-as-image (&optional (type "png"))
  (interactive)
  (plain-error "まだ実装していません。"))

;;
;; rabbit の実行 (private)
;;

(defun rabbit-execute-rabbit (file encoding theme &rest opts)
  (if file
      (rabbit-execute-rabbit-file file encoding theme opts)
    (rabbit-execute-rabbit-argf (buffer-substring (point-min) (point-max))
                                encoding theme opts)))

(defun rabbit-execute-rabbit-file (file encoding theme &optional opts)
  (when (zerop (file-length file))
    (plain-error (concat "ファイルが空なので rabbit を実行できません: " file)))
  (rabbit-make-process (format nil "~A ~{~A ~} ~A"
                               *rabbit-command*
                               (rabbit-option "file" encoding theme opts)
                               file)
                       (rabbit-init-output-buffer)
                       encoding))

(defun rabbit-execute-rabbit-argf (str encoding theme &optional opts)
  (when (zerop (length str))
    (plain-error "文字列が空なので rabbit を実行できません"))
  (let ((proc (rabbit-make-process (format nil "~A ~{~A ~}"
                                           *rabbit-command*
                                           (rabbit-option "argf" encoding theme opts))
                                   (rabbit-init-output-buffer)
                                   encoding)))
    (process-send-string proc str)
    (rabbit-process-send-eof proc)
    proc))

(defun rabbit-process-send-eof (proc)
  (process-send-string proc (format nil "~c" #\C-z)))

(defun rabbit-option (type encoding theme &optional opts)
  (let ((result opts))
    (when type
      (push (concat "--type " type) result))
    (when theme
      (push (concat "--theme " theme) result))
    (when (rabbit-encoding encoding)
      (push (concat "--encoding " (rabbit-encoding encoding)) result))
    result))

;;
;; output
;;

(defun rabbit-buffer-filename ()
  (get-buffer-file-name (selected-buffer)))

(defun rabbit-encoding (encoding)
  (let ((enc (char-encoding-name encoding)))
    (if (string-matchp "^utf" enc)
        nil
      enc)))

(defmacro rabbit-with-marker-point (marker &rest body)
  `(progn
     (goto-marker ,marker)
     ,@body
     (set-marker ,marker (point))))
(setf (get 'rabbit-with-marker-point 'lisp-indent-hook) 1)

(defmacro rabbit-with-buffer-selected (buffer &rest body)
  `(save-excursion
     (set-buffer ,buffer)
     ,@body))
(setf (get 'rabbit-with-buffer-selected 'lisp-indent-hook) 1)

(defvar *rabbit-buffer-name* "*rabbit:output*")
(defvar *rabbit-buffer* nil)
(defun rabbit-make-process (command-line output encoding)
  (message command-line)
  (let ((proc (make-process command-line :output output :outcode encoding)))
    (set-process-filter proc 'rabbit-process-filter)
    (set-process-sentinel proc 'rabbit-process-sentinel)
    (rabbit-activate-buffer output)
    proc))

(defun rabbit-init-output-buffer ()
  (save-excursion
    (when (or (not *rabbit-buffer*)
              (deleted-buffer-p *rabbit-buffer*))
      (let ((buf (get-buffer-create *rabbit-buffer-name*)))
        (set-buffer buf)
        (make-local-variable 'need-not-save)
        (setf need-not-save t)
        (setf *rabbit-buffer* buf)))
    (erase-buffer *rabbit-buffer*)
    *rabbit-buffer*))

(defvar *rabbit-output-buffer-height* 1/10)
(defun rabbit-activate-buffer (buf)
  (unless (zerop (buffer-size buf))
    (pop-to-buffer buf (if (< *rabbit-output-buffer-height* 1)
                           (ceiling (* (screen-height) *rabbit-output-buffer-height*))
                         *rabbit-output-buffer-height*))))

(defun rabbit-process-filter (proc str)
  (rabbit-with-buffer-selected *rabbit-buffer*
   (rabbit-with-marker-point (process-marker proc)
                             (insert str)))
  (rabbit-activate-buffer *rabbit-buffer*))

(defun rabbit-process-sentinel (proc)
  (when (and *rabbit-buffer*
             (zerop (buffer-size *rabbit-buffer*)))
    (delete-buffer *rabbit-buffer*)
    (setf *rabbit-buffer* nil)))

;;
;; template
;;

(defun rabbit-insert-title-template ()
  (interactive)
  (save-excursion (insert *rabbit-title-template*))
  (forward-char 2))

;;
;; スライド単位での編集とか
;;

(defun rabbit-mark-slide ()
  (interactive)
  (multiple-value-bind (beg end)
      (rabbit-current-slide-point)
    (goto-char end)
    (set-mark)
    (goto-char beg)))

(defun rabbit-toggle-hide-slide ()
  "rabbit-mode: 非表示スライドをトグルする。単純にコメントアウトするだけ"
  (interactive)
  (multiple-value-bind (beg end)
      (rabbit-current-slide-point)
    (save-excursion
      (save-restriction
        (narrow-to-region beg (1- end)) ; end は次のスライドの行頭
        (goto-char beg)
        (if (rabbit-hide-slide-p)
            (replace-buffer "^#" "" :regexp t)
          (replace-buffer "^" "#" :regexp t))))))

(defun rabbit-hide-slide-p ()
  "rabbit-mode: 非表示スライドなら non-nil。"
  (save-excursion
    (goto-char (rabbit-current-slide-point))
    (char= #\# (following-char))))

(defun rabbit-move-up-slide ()
  "rabbit-mode: スライドを上に移動する。"
  (interactive)
  (let ((slide (rabbit-current-slide)))
    (rabbit-kill-slide)
    (rabbit-backward-slide)
    (rabbit-insert-slide slide)))

(defun rabbit-move-down-slide ()
  "rabbit-mode: スライドを下に移動する。"
  (interactive)
  (let ((slide (rabbit-current-slide)))
    (rabbit-kill-slide)
    (rabbit-forward-slide)
    (rabbit-insert-slide slide)))

(defun rabbit-copy-current-slide ()
  "rabbit-mode: 現在のスライドをコピーする。"
  (interactive)
  (let ((slide (rabbit-current-slide)))
    (rabbit-forward-slide)
    (rabbit-insert-slide slide)))

(defun rabbit-copy-slide ()
  "rabbit-mode: スライド一覧からスライドをコピーする。"
  (interactive)
  (let ((slide (save-excursion
                 (and (list-function) (rabbit-current-slide)))))
    (when slide
      (rabbit-forward-slide)
      (rabbit-insert-slide slide))))

(defun rabbit-insert-slide (slide)
  (unless (bolp) (insert #\LFD))
  (save-excursion (insert slide)))

(defun rabbit-forward-slide (&optional (no-dup t))
  (interactive)
  ; forward-paragraph だとタイトル行の行頭以外にいる場合、ひとつ前のスライドまで戻ってしまう。
  ; この場合行頭に行ってほしい。
  (or (scan-buffer *rabbit-slide-header-regexp* :regexp t :no-dup no-dup)
      (end-of-buffer)))

(defun rabbit-backward-slide (&optional (no-dup t))
  (interactive)
  (or (scan-buffer *rabbit-slide-header-regexp* :regexp t :no-dup no-dup :reverse t)
      (beginning-of-buffer)))

(defun rabbit-current-slide ()
  (rabbit-funcall-with-current-point 'buffer-substring))

(defun rabbit-narrow-to-slide ()
  (interactive)
  (rabbit-funcall-with-current-point 'narrow-to-region))

(defun rabbit-delete-slide ()
  (interactive)
  (rabbit-funcall-with-current-point 'delete-region))

(defun rabbit-kill-slide ()
  (interactive)
  (kill-new (rabbit-current-slide))
  (rabbit-delete-slide))

(defun rabbit-title-slide ()
  (multiple-value-bind (beg end)
      (rabbit-title-slide-point)
    (buffer-substring beg end)))

(defun rabbit-title-slide-point ()
  (save-excursion
    (goto-char (point-min))
    (values (progn (rabbit-forward-slide nil) (point))
            (progn (rabbit-forward-slide) (point)))))

(defun rabbit-funcall-with-current-point (fn)
  (multiple-value-bind (beg end)
      (rabbit-current-slide-point)
    (funcall fn beg end)))

(defun rabbit-current-slide-point ()
  (values (save-excursion (rabbit-backward-slide nil) (point))
          (save-excursion (rabbit-forward-slide) (point))))

;;
;; list-function で表示するスライドの一覧
;;

(defun rabbit-build-summary-of-function ()
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (while (scan-buffer *rabbit-slide-header-regexp* :regexp t :tail t)
	(push (list (current-line-number) (match-string 1)) result)))
    (unless result
      (plain-error "ないっす"))
    (nreverse result)))

;;
;; 補完
;;

(defvar *rabbit-mode-abbrev-table* nil)
(unless *rabbit-mode-abbrev-table*
  (define-abbrev-table '*rabbit-mode-abbrev-table*))

;;
;; mode の実行
;;
(defun rabbit-mode ()
  "rabbit-mode"
  (interactive)
  (kill-all-local-variables)
  (setf mode-name "Rabbit")
  (setf buffer-mode 'rabbit-mode)
  (use-syntax-table *rabbit-mode-syntax-table*)
  (use-keymap *rabbit-mode-map*)

  ; 略語展開
  (setf *local-abbrev-table* *rabbit-mode-abbrev-table*)

  (make-local-variable 'paragraph-start)
  (setf paragraph-start *rabbit-slide-header-regexp*)
  (make-local-variable 'paragraph-separate)
  (setf paragraph-separate paragraph-start)

  ; list-funcations
  (make-local-variable 'build-summary-function)
  (setf build-summary-function 'rabbit-build-summary-of-function)

  (setf comment-start "# " comment-end "") ; indent-for-commentで挿入される奴
  (setf comment-start-skip "#+[ \t]*")
  (when *rabbit-comment-column*
    (setf comment-column *rabbit-comment-column*))

  ; keyword
  (make-local-variable 'regexp-keyword-list)
  (setf regexp-keyword-list *rabbit-regexp-keyword-list*)

  ; ローカルメニューの設定
  (insert-rabbit-menu)

  ; フックの実行
  (run-hooks '*rabbit-mode-hook*))
