---
layout: ja
title: Windowsへインストール
---
== Windowsでのインストール方法

Windows上でRabbitをインストール、利用する手順について説明し
ます。

必ずインストールしなければいけないソフトウェアと、なくても動
くけどあると便利なソフトウェアがあります。

=== 必須のソフトウェアのインストール

以下が必須のソフトウェアです。

  * Ruby
  * Rabbit

まずは必須のソフトウェアをインストールしましょう。

==== Ruby 本体のインストール

((<RubyInstall for Windowsのダウンロードページ（英語）
|URL:http://rubyinstaller.org/downloads/>))からRuby 2.0.0 の
インストーラをダウンロードします。Ruby 2.0.0-p451のインストー
ラは以下のURLからダウンロードできます。

: rubyinstaller-2.0.0-p451.exe
   ((<URL:http://dl.bintray.com/oneclick/rubyinstaller/rubyinstaller-2.0.0-p451.exe>))

==== Rabbitのインストール

RubyInstallerをインストールするとスタートメニューに「Start
Command Prompt with Ruby」（日本語の場合は「Ruby コマンド
プロンプトを開く」）というプログラムが追加されます。こ
のプログラムを実行するとruby.exeにパスが通ったコマンドプロン
プトが表示されます。ここで以下のコマンドを実行することで
Rabbitをインストールできます。Ruby/GTK2など関連するソフトウェ
アも一緒にインストールされます。

  > gem install rabbit

=== 必須ではないソフトウェアのインストール

以下がなくても動くけどあると便利なソフトウェアです。

  * RDtool
  * Ghostscript
  * Enscript
  * div

それではあると便利なソフトウェアをインストールしましょう。

==== RDTool のインストール

((<RAA:rdtool>)) をインストールする。

アーカイブを展開して，setup.rb を実行してインストールする．

: rdtool-0.6.20.tar.gz
   ((<URL:http://www.moonwolf.com/ruby/archive/rdtool-0.6.20.tar.gz>))

==== EPS を表示する

(1) AFPL Ghostscript 8.53 for Win32 をインストールする。

    : gs853w32.exe
        ((<URL:http://www.cs.wisc.edu/~ghost/>))

(2) インストール後 <インストール先>/gs/gs8.53/bin に PATH を通す。

==== ソースコードの色づけを行う

(1) Enscript をインストールする。

    : enscript-1.6.3-9-bin.exe
        ((<URL:http://sourceforge.net/project/showfiles.php?group_id=23617&package_id=16960>))

(2) <Enscript のインストール先>/bin に PATH を通す。

(3) ruby コードを色づけする場合は、以下のコマンドで ruby.st もインストールする。

      > cd <Enscript のインストール先>\share\enscript\hl
      > ruby -ropen-uri -e "puts open('http://viewvc.rubyforge.mmmultiworks.com/cgi/viewvc.cgi/trunk/support/ruby.st?root=support&view=co').read" > ruby.st

(4) ((<RAA:htree>)) をインストールする。

    アーカイブを展開して，install.rb を実行してインストールする。

    : htree.tar.gz
        ((<URL:http://cvs.m17n.org/viewcvs/ruby/htree.tar.gz>))

==== rabrick を使う

((<RAA:div>)) をインストールする。

アーカイブを展開して，install.rb を実行してインストールする。

: div-1.3.2.tar.gz
   ((<URL:http://www2a.biglobe.ne.jp/~seki/ruby/div-1.3.2.tar.gz>))
