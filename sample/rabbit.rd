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

  * 実装: Ruby/GTK+ 2/cairo
  * 動作: PC-UNIX/Win/Mac
  * 書式:\n
    RD/Wiki/Markdown/PDF
  * 見た目: Ruby(('note:（ソースと分離）'))

= 機能: 表示（1）

  * ((*強調*))・(('del:削除'))
  * 下付き(('sub:文字'))・上付き(('sup:文字'))
  * 数式記号: (('&sum;'))(('sub:i=0'))(('&sum;sub:i=0'))
  * ソースの色付け

= 機能: 表示（2）

  * 表
  * おもしろテーマ
  * 画像
    * 対応フォーマット多数
    * PNG/JPEG/.../PDF/EPS/SVG

= 機能: 表示（3）

  * 長い行の折り畳み
  * ソースの色付け
  * 大きな文字

= 機能: UI（1）

  * 豊富なキーバインド
  * 右クリックメニュー
  * マウスジェスチャ
  * スポットライト
  * 虫眼鏡

= 機能: UI（2）

  # wait
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
  * Hiki
  * SlideShare

= 機能: 入力書式

  * RD
  * Wiki（Hiki）
  * Markdown（kramdown）
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

= 機能: タグ

(('tag:x-large:大きなテキスト'))

(('tag:center'))中央寄せされたテキスト

(('tag:right'))右寄せされたテキスト

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
#  # relative_width = 100
#  # relative_height = 50

= 画像: 鏡面反射

  # image
  # src = shocker.jpg
  # relative_height = 80
  # reflect_ratio = 0.5

= 画像: 背景（1）

  * 背景画像
  * デフォルトはセンタリング

== プロパティ

: background-image
   lavie.png

: background-image-relative-width
   50

# : background-image-align
#    right

# : background-image-relative-margin-right
#    3

= 画像: 背景（2）

    # image
    # src = lavie.png
    # relative-width = 30
    # align = right
#    # vertical-align = top
    # relative-margin-right = -5

  * 右寄せ背景画像
  * スライド内で指定
    * align = right

= 画像: 落書き

  # image
  # src = lavie.png
  # relative_height = 80
  # draw0 = [rectangle, false, 0.05, 0.1, 0.1, 0.2, "red", {line_width: 3}]
  # draw1 = [line, 0.2, 0.1, 0.3, 0.2, "blue", {line_width: 5}]
  # draw2 = [text, Cute!, 0.05, 0.4, "Green", {size: 25}]

= 画像サイズ

スライドサイズに応じて変更可能

  # image
  # src = usagi.png
  # caption = 兎
#  # relative_width = 100
  # relative_height = 80

= 外部画像

URL先の画像をダウンロード

  # image
  # src = https://raw.githubusercontent.com/rabbit-shocker/rabbit/master/data/rabbit/image/cozmixng-images/cozmixchu.png
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

= aafigure

  # aafigure
  # relative_width = 90
#  # foreground = #ff3333
#  # option = font=mikachan
  あいうえお
  A   B   C   D   E   F   G   H   I   J   K   L   M
   AA  BB  CC  DD  EE  FF  GG  HH  II  JJ  KK  LL  MM
   AA  BB  CC  DD  EE  FF  GG  HH  II  JJ  KK  LL  MM

   aa  bb  cc  dd  ee  ff  gg  hh  ii  jj  kk  ll  mm
   aa  bb  cc  dd  ee  ff  gg  hh  ii  jj  kk  ll  mm

  N   O   P   Q   R   S   T   U   V   W   X   Y   Z
   NN  OO  PP  QQ  RR  SS  TT  UU  VV  WW  XX  YY  ZZ
   NN  OO  PP  QQ  RR  SS  TT  UU  VV  WW  XX  YY  ZZ

   nn  oo  pp  qq  rr  ss  tt  uu  vv  ww  xx  yy  zz
   nn  oo  pp  qq  rr  ss  tt  uu  vv  ww  xx  yy  zz

= blockdiag

  # blockdiag
  # relative_width = 90
#  # antialias = true
  # fonts = /usr/share/fonts/opentype/ipafont/ipag.ttf, /usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf
  {
    default_fontsize = 25;
    RD -> Rabbit;
    Hiki -> Rabbit;
    PDF -> Rabbit;
    group {
      Rabbit -> 画面;
    }
  }

= 折り返し

なーーーーーーーーーーーーーーーーーーーーーーーーーーーーーがーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーい行

= ソース

以下がソース

  # comment
  def method_name
    body
  end

以上がソース

= ソース: Enscript

以下がソース

  # enscript ruby
  # comment
  def method_name
    body
  end

以上がソース

= ソース: CodeRay

以下がソース

  # coderay ruby
  # comment
  def method_name
    body
  end

以上がソース

= ソース: Emacs

以下がソース

  # emacs # -*- ruby -*-
  # comment
  def method_name
    body
  end

以上がソース

= ソース: Rouge

以下がソース

  # rouge ruby
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

= 操作: 移動

: 次ページ
   次に進みそうなキー/左クリック

   n, f, j, l, Spc, Ret, +, (('&DownArrow;')), (('&RightArrow;')), ...

: 前ページ
   前に進みそうなキー/中クリック

   p, b, k, h, BS, Del, -, (('&UpArrow;')), (('&LeftArrow;')), ...

= 操作: 高度な移動

: タイトルページへ移動
   a, 0, <, Home

: nページ目へ移動
   1-9, +Ctrl = +10, +Alt = +20

: 最後のページへ移動
   e, $, >, End

= 操作: 本番用（1）

: フルスクリーン切替え
   F5, F10, F11, ジェスチャ(('&DownArrow;'))(('&UpArrow;'))

: 一覧モード切替え
   i

: 一覧モードからページ移動
   ダブルクリック

= 操作: 本番用（2）

: キャッシュ作成
   c

: 情報ウィンドウ表示切替え
   I

= 操作: 本番用（3）

: 虫眼鏡
   Ctrl+右クリック

   ホイールで拡大率変更

: スポットライト
   ダブル右クリック

   ホイールで半径変更

= 操作: 本番用（4）

: 落書き
   ポップアップ（右クリック）→\n落書きモード

: マウスジェスチャー
   右ドラッグ

= 操作: 本番用（5）

: ホワイトアウト
   W

: ブラックアウト
   B

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
  * 機能/UI: 高機能/ユニーク
  * キーボード重視
    * UI・テキストベースのソース
