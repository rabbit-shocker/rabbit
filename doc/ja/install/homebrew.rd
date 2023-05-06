---
layout: ja
title: Homebrewでインストール
---
== macOSでHomebrewを使ったインストール方法

macOS のパッケージ管理システムのひとつである Homebrew と RubyGems を利用して
Rabbit をインストール、利用する手順について説明します。

=== 事前に準備するもの

* macOS
* Xcode と X11（XQuartzをインストール）

=== Homebrew のインストール

((<URL:https://brew.sh/>)) に記載の方法に従ってインストールしてください。

==== brew install の実行

  $ brew install cairo
  $ brew install pango
  $ brew install gtk+
  $ brew install gobject-introspection
  $ brew install poppler

=== Rabbit のインストール

gem install を実行します。

  $ sudo gem install rabbit
  $ sudo gem install rabbiter # Twitter連携機能を使う場合

=== 環境変数の設定

~/.bash_loginあるいは~/.zshenvで以下のようにDYLD_LIBRARY_PATH環境変数を設定します。

  export DYLD_LIBRARY_PATH=/usr/local/opt/cairo/lib
