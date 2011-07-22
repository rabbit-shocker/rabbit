---
layout: ja
title: テーマの作り方
---
== テーマについて

スライドの見た目はRDとは別に指定します．それがテーマです．

テーマのテスト用のスライドがsample/theme-bench.rabにあるので
活用してください。

== 置き場所

テーマはrubyの$LOAD_PATHから検索されます．新しくテーマを作成
した場合は((|$LOAD_PATHに含まれているパス/rabbit/theme/テーマ
名/テーマ名.rb|))あるいは((|ソースがあるディレクトリ/テーマ
名.rb|))に置いてください．

== 書き方

テーマはRubyスクリプトです．難しいと思われるかもしれませんが，
凝ったことをしなければそれほど難しいことはありません．

テーマは以下のような記述を列挙していくことになると思います．

  (1) 対象となるスライド中の要素（段落とか，ページタイトルと
      か）を選択

  (2) 選択した要素に対して属性を変更したり，描画時の動作を加
      えたりする．

例えば，各ページ（タイトルページは含まない）にあるタイトルの
文字の色を変更したい場合は以下のように書きます．

  match(Page, HeadLine) do |heads|
    heads.prop_set("foreground", "red")
  end

((|heads|))というように複数形になっているのは，((|heads|))は
タイトル（見出し，head line）を0個以上含んでいるからです．

== プロパティ

(({prop_set}))では前景色（foreground）以外にも以下のものが指
定できます．詳しくは((<Pango Text Attribute
Markup|URL:http://developer.gnome.org/pango/stable/PangoMarkupFormat.html>))
を見てください．

: font_desc
   フォント情報を指定します．

: font_family
   フォント名を指定します．
   
   フォント名の一覧は(({font_families}))とやればフォント名の
   配列として取得できます．

: face
   font_familyと同じです．

: size
   フォントの大きさを指定します．
   
   フォントの大きさを数値で指定する場合は，指定する数値を
   (({screen_size}))で変換してから(({Pango::SCALE}))を掛けて
   ください．例えば，フォントの大きさを（Rabbitのテーマの世
   界の単位で）2にする場合は以下のようにします．

     screen_size(2) * Pango::SCALE

: style
   フォントのスタイルを指定します．

: weight
   フォントの重みを指定します．

: variant
   フォントのvariant（変形方法って感じ？）を指定します．

: stretch
   フォントの伸び縮みの具合を指定します．

: foreground
   前景色を指定します．

: background
   背景色を指定します．

: underline
   下線の種類をします．

: rise
   文字の上下の位置を指定します．

: strikethrough
   取消線を引くかどうかを指定します．

: fallback
   指定したフォントがなかった場合に似たようなフォントで代用
   するかどうかを指定します．

: lang
   言語を指定します．

: b
   太字にします．

: big
   フォントのサイズを大きくします．

: i
   斜体にします．

: s
   取消線を引きます．

: sub
   下付き文字にします．

: sup
   上付き文字にします．

: small
   フォントのサイズを小さくします．

: tt
   固定スペースフォントにします．

: u
   下線を引きます．

== フック

要素が描画される前，又は後に実行される手続きを指定することが
できます．それぞれ，(({add_pre_draw_proc}))，
(({add_post_draw_proc}))で指定できます．これらの手続きを削除
するときは，それぞれ(({clear_pre_draw_procs}))，
(({clear_post_draw_procs}))を使います．

詳しくは．．．とりあえず，今のところはdefaultテーマを参考に
してください．

