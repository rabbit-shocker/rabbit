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
|URL:http://rubyinstaller.org/downloads/>))からRuby 2.0.0 以降の
インストーラをダウンロードします。Ruby 2.0.0-p576のインストー
ラは以下のURLからダウンロードできます。

: rubyinstaller-2.0.0-p576.exe
   ((<URL:http://dl.bintray.com/oneclick/rubyinstaller/rubyinstaller-2.0.0-p576.exe>))

注意：64bit版Windowsを使っている場合でも、32bit版のRubyをインストールしてください。
現在のところ、RabbitはWindows上の64bit版Rubyでは動作しません。

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

  * Ghostscript

それではあると便利なソフトウェアをインストールしましょう。

==== EPS を表示する

(1) AFPL Ghostscript 8.53 for Win32 をインストールする。

    : gs853w32.exe
        ((<URL:http://www.cs.wisc.edu/~ghost/>))

(2) インストール後 <インストール先>/gs/gs8.53/bin に PATH を通す。
