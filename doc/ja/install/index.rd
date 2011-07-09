---
layout: ja
title: インストール
---
== インストール方法について

Rabbitはgemまたはそれぞれのプラットフォームにあるパッケージ管
理システムを使ってインストールすることができるため、導入が非
常に簡単です。

== gemを使う方法

  % gem install rabbit
  % gem install twitter-stream # If you want to use Twitter related features
  % gem install twitter_oauth  # If you want to use Twitter related features

== Debian GNU/Linuxでのインストール方法

  % sudo aptitude install -y rabbit

== Ubuntuでのインストール方法

  % sudo aptitude install -y rabbit

== Gentoo Linuxでのインストール方法

  % sudo env ACCEPT_KEYWORDS=~x86 FEATURES="digest" emerge rabbit

== NetBSD（またはpkgsrcが使えるプラットフォーム）でのインストール方法

  % sudo pkg_add ruby18-rabbit

または

  % sudo pkg_add ruby19-rabbit

== FreeBSDでのインストール方法

  % sudo portupgrade -NRr rabbit

== Mac OS XでMacPortsを使ったインストール方法

((<macports.rd/MacPortsを使ったインストール方法>))を参照してく
ださい。

== Mac OS XでHomebrewを使ったインストール方法

((<homebrew.rd/Homebrewを使ったインストール方法>))を参照してく
ださい。

== Windowsでのインストール方法

((<windows.rd/Windowsでのインストール方法>))を参照してくださ
い。

== tar.gzからインストール

((<URL:http://rabbit-shockers.org/download/rabbit.tar.gz>))
から最新のRabbitをダウンロードし、setup.rbを実行します。この
URLは常に最新のRabbitのアーカイブになります。

  % mkdir tmp
  % cd tmp
  % wget http://rabbit-shockers.org/download/rabbit.tar.gz
  % tar xvzf rabbit.tar.gz
  % cd rabbit-*
  % sudo ruby setup.rb
