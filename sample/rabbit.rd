# To run with it without system install.
#  % ruby -I./lib bin/rabbit -f sample/rabbit.rd 
# $Id$

= Rabbit

: subtitle
   RDでプレゼンテーション
: author
   須藤功平
: institution
   COZMIXNG
#: content_source
#   出典
: theme
   rabbit

= Rabbit

プレゼンテーションツール

  * 言語: Ruby
  * バックエンド: GTK+ 2/cairo
  * 動作: PC-UNIX/Win/Mac
  * 書式: RD/Wiki/PDF
  * 見た目: Ruby(('note:（ソースと分離）'))

= 機能: 表示（1）

  * ((*強調*))・(('del:削除'))
  * 下付き(('sub:文字'))・上付き(('sup:文字'))
  * 数式記号: (('&sum;'))(('sub:i=0'))(('&sum;sub:i=0'))
  * ソースの色付け
  * 表

= 機能: 表示（2）

  * おもしろテーマ
  * 画像
    * 対応フォーマット多数
    * PNG/JPEG/.../PDF/EPS/SVG
  * 長い行の折り畳み

= 機能: UI（1）

  * 豊富なキーバインド
  * 右クリックメニュー
  * マウスジェスチャ
  * スポットライト
  * 虫眼鏡

= 機能: UI（2）

  * 一覧ページ
  * 落書き
  * (('wait'))ポーズ
  * (('wait'))国際化
  * スライド内検索

= 機能: UI（3）

  * ホワイト・ブラックアウト
    * スライド白塗り・黒塗り
  * ラビットホール
    * スライドに穴
  * 持ち時間の残りを視覚化
    * うさぎとかめ

= 機能: 入力

  * ファイル
  * 標準入力
  * HTTP
  * RWiki/Hiki
  * SlideShare

= 機能: 入力書式

  * RD
  * Wiki（Hiki）
  * PDF
    * (('&RightArrow;'))PDFビューア

= 機能: 出力

  * 画像
  * 画像 + HTML
  * PS/PDF
  * 印刷用PS/PDF
    * nスライド/ページ

= 機能: 外部API

  * HTTP
  * dRuby
  * XML-RPC
  * SOAP

= 機能: 作成支援

  * ソースの自動再読み込み
  * テーマの再読み込み
  * テーマの切り替え

= ToDo

  * インラインでの画像表示
  * リンク先へのジャンプ
  * サウンド
  * ビデオ
  * 3D

= 画像

  # image
  # src = lavie.png
  # caption = Lavie
  # width = 100
  # height = 100
#  # normalized_width = 50
#  # normalized_height = 50
#  # relative_width = 100
#  # relative_height = 50

= 画像サイズ

スライドサイズに応じて変更可能

  # image
  # src = usagi.png
  # caption = 兎
#  # normalized_width = 50
#  # normalized_height = 50
#  # relative_width = 100
  # relative_height = 50

= 外部画像

URL先の画像をダウンロード

  # image
  # src = http://www.cozmixng.org/repos/images/cozmixchu.png
  # caption = こずみっくちゅー

= 数式

  * 書式: TeX(('note:（っぽい）'))
  * バックエンド
    * LaTeX
    * mimeTeX

= LaTeX

  # LaTeX
  # relative_width = 80

  $f(x)=\displaystyle\int_{-\infty}^x~e^{-t^2}dt$

  \LaTeX

= mimeTeX

  # mimeTeX
  # relative_width = 80

  \Large f(x)=\Bigint_{-\infty}^x~e^{-t^2}dt

= EPS

事前にEPSで作成((-gsが必要-))

  # image
  # src = equation.eps
  # relative_width = 80

= SVG

  # image
  # src = spiral.svg
  # relative_height = 100

= Dia

  # image
  # src = rabbit.dia
  # relative_width = 90

= GIMP

  # image
  # src = rabbit.xcf
  # relative_height = 100

= 折り返し

なーーーーーーーーーーーーーーーーーーーーーーーーーーーーーがーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーい行

= ソース

以下がソース

  # comment
  def method_name
    body
  end

以上がソース

= ソース: 色付き

以下がソース

  # enscript ruby
  # comment
  def method_name
    body
  end

以上がソース

= 引用

  # blockquote
  # title = The Matrix
  You take the ((*red pill*)), you stay in Wonderland and 
  I show you how deep the ((*rabbit-hole*)) goes.

= 箇条書き

  (1) レベル1-1

      (1) レベル2-1

          (1) レベル3-1

          (1) レベル3-2

      (1) レベル2-2

  (1) レベル1-2


= ラベル付きリスト

: Rabbit
   うさぎ

   : Tortoise
      かめ

: うさぎ
   Rabbit

= 表

  # RT
  caption = 表のサンプル

  みだし1, みだし2

  内容1, 内容2
  長ーーーーーい内容3, 長ーーーーーーーーーーーーーーーーーーい内容4

= Anthy

ひらがなをかんじにへんかんできます．

いみもつかいみちもないです．


((*変換後:*))

  # anthy
  ひらがなをかんじにへんかんできます．

  いみもつかいみちもないです．

= 操作: 移動

: 次ページ
   次に進みそうなキー/左クリック

   n, f, j, l, Space, Return, +, (('&DownArrow;')), (('&RightArrow;')), ...

: 前ページ
   前に進みそうなキー/真ん中クリック

   p, b, k, h, BS, Delete, -, (('&UpArrow;')), (('&LeftArrow;')), ...

= 操作: 高度な移動

: タイトルページへ移動
   a, 0, <, Home

: nページ目へ移動
   1-9, +Ctrl = +10, +Alt = +20

: 最後のページへ移動
   e, $, >, End

= 操作: 本番用（1）

: フルスクリーン切替え
   F5, F10, F11, マウスジェスチャ(('&DownArrow;'))(('&UpArrow;'))

: 一覧モード切替え
   i

: 一覧モードからページ移動
   ダブルクリック

= 操作: 本番用（2）

: キャッシュ作成
   c

: 情報ウィンドウ表示切替え
   I

= 操作: 保存

: スクリーンショット
   各ページを画像として保存

   s

: 印刷
   各ページをPS/PDFとして保存

   Ctrl+p

= 操作: 描画

: 再描画
   Ctrl+l

: テーマ再読み込み
   t, r

: スライドの調整値をリセット
   Alt+a

= 操作: 穴

: 穴を広げる
   E

: 穴を狭める
   N

= 操作: 検索

: 次を検索
   C-s, /

: 前を検索
   C-r, ?

: 検索終了
   C-g

= 操作: 終了

: 終了
   q, Escape

: アイコン化
   z

= まとめ

  * プレゼンテーションツール
  * マルチプラットフォーム
  * 機能・UI: 高機能・ユニーク
  * キーボード重視
    * UI・テキストベースのソース
