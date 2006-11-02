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

= 何これ？

プレゼンテーションツール

  * Rubyで書かれている
  * GTK+ 2を使っている
    * 使ってみたかったんだもん
      * だめ？(('note:いいじゃん！'))

= 特長

  * ソースをRDで書ける
  * RWikiと協調する
    * ソースの編集はRWikiで行う
    * プレゼンするときはRWikiから直接ソースをもってくる
  * 見た目はRDとは別に指定

= できる(('sup:up'))こと(('sub:down'))

  * テキストを((*強調*))し(('del:なかっ'))たり
  * 下付き(('sub:文字'))とか上付き(('sup:文字'))とか．
  * 数式記号とか(('&sum;'))(('sub:i=0'))(('&Sigma;&sum;'))(('&sum;sub:i=0'))
  * 画像表示

= まだある

  * 変更されたソースの自動再読み込み
  * テーマの再読み込み
  * Indexページの自動生成
  * 右クリックメニュー

= まだまだある

  * オフスクリーンでのスクリーンショット作成
  * 国際化されたメッセージ
  * PS/PDF出力((-品質は微妙-))

= まだまだまだある

  * 表
  * ソースの色付け
  * おもしろテーマ
  * スライドに落書き

= まだ(('sup:4'))ある

  * スクリーンショットの整形
  * マウスジェスチャ
  * スポットライト
  * 虫眼鏡

= できないこと

  * インラインでの画像挿入
  * リンク先へのジャンプ

= ToDo

  * 音を鳴らす
  * 3Dの絵を書く（X3Dのライブラリがあれば．．．）
    * OpenGLもサポートしているので書けるといえば書ける．

= 求めているもの

  * coolなテーマ
  * greatなドキュメント

= スライド保存

  * スライドを画像として保存
  * スライド表示用のシンプルなHTMLも出力

= 画像は？

  * インラインでなければ大丈夫．

    サイズ変更も可能

      # image
      # src = lavie.png
      # caption = Lavie
      # width = 100
      # height = 100
#      # normalized_width = 50
#      # normalized_height = 50
#      # relative_width = 100
#      # relative_height = 50

= 画像サイズは？

スライドサイズに応じて変更可能

  # image
  # src = usagi.png
  # caption = 兎
#  # normalized_width = 50
#  # normalized_height = 50
#  # relative_width = 100
  # relative_height = 50

= ローカルにない画像は？

  * 外部URLも大丈夫

      # image
      # src = http://www.cozmixng.org/repos/images/cozmixchu.png
      # caption = こずみっくちゅー

= 数式は？

  * 書式はTeX（っぽい）
  * バックエンド
    * LaTeX
    * Tgif
    * mimeTeX

= LaTeX

  # LaTeX
  # relative_width = 80

  $f(x)=\displaystyle\int_{-\infty}^x~e^{-t^2}dt$

  \LaTeX

= Tgif

  # Tgif
  # relative_width = 80

  large f(x)=int {-infty} x~e^{-t^2}d t

= mimeTeX

  # mimeTeX
  # relative_width = 80

  \Large f(x)=\Bigint_{-\infty}^x~e^{-t^2}dt

= あるいはEPSを使う

  * あらかじめEPSを作っておく

    * でも，gsがなきゃだめなの．

  # image
  # src = equation.eps
  # relative_width = 80

= SVGでもOK

  # image
  # src = spiral.svg
  # relative_height = 100

= TgifでもOK

  # image
  # src = rabbit-balloon.obj
  # relative_height = 100

= DiaでもOK

  # image
  # src = rabbit.dia
  # relative_width = 90

= GIMPでもOK

  # image
  # src = rabbit.xcf
  # relative_height = 100

= 折り返し

なーーーーーーーーーーーーーーーーーーーーーーーーーーーーーがーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーい行は？

= ソースをのせると？

どう？

  # こんなかんじに
  def なり
    ますよ
  end

どう？

= 色がつくかも

どう？

  # enscript ruby
  # こんなかんじに
  def なり
    ますよ
  end

どう？

= 箇条書きは？

  (1) どう

  (1) です

      (1) どうどうどう

          (1) もう

          (1) いっちょーーー

      (1) どうなのよぉ

  (1) か？


= ラベル付きリストは？

: Rabbit
   うさぎ

   : Tortoise
      かめ

: うさぎ
   Rabbit

= 表は？

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

= テーマの書き方

--- Rabbit::Theme#match(*path, &block)
     パスでテーマを適用する要素を指定する

うーん，後でちゃんとしたものを用意します．

= キーバインド（基本）

: 次ページ
   n, f, j, l, Spc, Ret, Tab, +, (('&DownArrow;')),
   (('&RightArrow;')), 左クリックなど

: 前ページ
   p, b, k, h, BS, Del, -, (('&UpArrow;')),
   (('&LeftArrow;')), 真ん中クリックなど

= キーバインド（基本2）

: 終了
   q, Esc

= キーバインド（便利）

: タイトルページへ移動
   a, 0, <, Home
: nページ目へ移動
   1-9．+Ctrlで+10，+Altで+20
: 最後のページへ移動
   e, $, >, End

= キーバインド（本番）

: フルスクリーン切替え
   F5, F10, F11

: 一覧モード切替え
   i

: 一覧モードからページ移動
   ダブルクリック

= キーバインド（機能）

: スクリーンショット
   各ページを画像として保存

   s

: 印刷
   各ページをPS/PDFとして保存

   Ctrl+p

= キーバインド（描画）

: 再描画
   Ctrl+l

: テーマ再読み込み
   t, r

: スライドの調整値をリセット
   Alt+a

= キーバインド（穴）

: 穴を広げる
   E

: 穴を狭める
   N

= キーバインド（検索）

: 次を検索
   C-s, /

: 前を検索
   C-r, ?

: 検索終了
   C-g

= キーバインド（その他）

: アイコン化
   z

: キャッシュ作成
   c

: 情報ウィンドウ表示切替え
   I

= おわり

こんな感じです．

書かなきゃいけないことはいろいろあるなぁ．

文書書きって苦手．
