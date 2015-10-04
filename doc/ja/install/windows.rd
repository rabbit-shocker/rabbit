---
layout: ja
title: Windowsへインストール
---
== Windowsでのインストール方法

Windows上でRabbitをインストールする方法、起動方法について説明します。

=== インストール方法

以下のソフトウェアをインストールします。

  * Ruby
  * Rabbit

それぞれ説明します。

==== Rubyのインストール方法

((<RubyInstall for Windowsのダウンロードページ（英語）
|URL:http://rubyinstaller.org/downloads/>))からインストーラーをダウン
ロードして実行します。32bit版のRubyでも64bit版のRubyでもかまいません。

==== Rabbitのインストール方法

RubyInstallerをインストールするとスタートメニューに「Start Command
Prompt with Ruby」（日本語の場合は「Ruby コマンドプロンプトを開く」）
というプログラムが追加されます。このプログラムを実行すると
(({ruby.exe}))にパスが通ったコマンドプロンプトが表示されます。ここで以
下のコマンドを実行することでRabbitをインストールできます。Ruby/GTK2な
ど関連するソフトウェアも一緒にインストールされます。

  > gem install rabbit

=== 起動方法

Rabbitのインストール時に使ったコマンドプロンプトで次のコマンドを実行し
ます。

  > rabbit

実行するとスライドファイルを開くダイアログが表示されるので作成したスラ
イドファイルを指定します。

スライドファイルの作成方法は((<スライドの作り方|URL:../how-to-make/>))
を参考にしてください。
