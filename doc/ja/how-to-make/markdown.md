---
layout: ja
title: Markdown記法でのスライドの作り方
---
## Rabbit用Markdown記法について

基本的にMarkdownの書式を使いますが、スライド用のMarkdownでは
マークアップの使い方が通常の文書の場合と異なります。

[サンプルスライド](../sample/)にサンプルがあります。

### ページ

一番大きな見出し「#」がページのタイトルになります。そのペー
ジは次の見出しまで続きます。

    # タイトル

    なにか

    ...

    # 次のページ

    ...

この例だと二ページになります。

通常のMarkdownと異なり、改行は無視されません。

### タイトルページ

最初のページはタイトルページになります。タイトルページには見
出し付きリスト「:」でスライドのメタ情報を指定できます。

    # 発表のタイトル

    author
    :    須藤功平
    institution
    :    COZMIXNG

この例では、作者が須藤功平で、所属がCOZMIXNGであるということ
を示しています。

以下の属性が指定可能です:

* author

* institution

* subtitle

* content_source

* date

* start_time

* end_time

* allotted_time

  プレゼンの持ち時間です。Rabbitを起動するときに指定しなかった場合に
  使用されます。`start_time`と`end_time`を両方指定している
  場合はそれらの属性から計算されるため、`allotted_time`を指定する
  必要はありません。

* theme

  テーマです。Rabbitを起動するときに指定しなかった場合に使用されます。

### 強調

\*を使って文字列を囲むと、強調することができます。

    *強調*

### 画像

通常のMarkdownと同様に記述できます。

    ![image](lavie.png)

### 表

表を組むこともできます。

    |みだし1       |みだし2       |みだし3         |
    |:-----------|------------:|:------------:|
    |内容1        |内容2         |内容3         |

### 箇条書き

Markdownのリスト記法のうち、\* \+ \- のいずれかを使えます。

    * レベル1-1
      * レベル2-1
    * レベル1-2
    * レベル1-3

### 引用

\>を行の先頭に置くと引用文になります。

    > You take the *red pill*, you stay in Wonderland and
    > I show you how deep the *rabbit-hole* goes.
