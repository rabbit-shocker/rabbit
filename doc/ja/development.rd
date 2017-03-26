---
layout: ja
title: 開発
---
== リポジトリ

Rabbitのリポジトリは
((<GitHub|URL:https://github.com/rabbit-shocker/rabbit/>))にあります。

== コミットメール

以下のメーリングリストにコミットメール毎に変更点が流れます。
メーリングリストに参加することで開発状況を確認できます。メー
リングリストに参加するには以下のようなメールを送信してくださ
い。

  To: rabbit@ml.commit-email.info
  Cc: null@commit-email.info
  Subject: 登録

  登録

== バグの報告方法

ご意見ご要望不具合報告等は作者へのメール、メーリングリスト（メーリング
リストについては((<users.rd/ユーザー>))ページを見てください）、
((<GitHubの
Issues|URL:https://github.com/rabbit-shocker/rabbit/issues>))をご利用く
ださい。

== 開発へ参加

Rabbitプロジェクトはみなさんが開発に参加してくれることを歓迎
します！

=== テーマ

新しいテーマを作ったら、ぜひメーリングリストなどで教えてくだ
さい。Rabbit本体に取り込みませんか？

=== ドキュメント

Rabbitのドキュメントが不足しています。まずは、あなたの知って
いることを((<faq.rd/FAQ>))ページに書いてみませんか？

慣れてきたら((<"how-to-make/"/スライドの作り方>))ページの内
容を充実させませんか？あるいは、既存の日本語のドキュメントを
英語に翻訳しませんか？

=== 開発環境の作り方

==== リポジトリをclone

((<"http://rabbit-shocker.org/"/本サイト>))のソースコードもRabbitと同じ((<リポジトリ|URL:https://github.com/rabbit-shocker/rabbit/>))にあります。Rabbitのリポジトリをクローンします。

  % git clone https://github.com/rabbit-shocker/rabbit.git

サイトのソースコードはdocディレクトリにあります。

==== Gemライブラリのインストール

開発に必要なライブラリをBundlerでインストールします。

  % cd rabbit
  % bundle install --path vendor/bundle

((<"http://bundler.io/"/Bundler>))はgemのパッケージ管理ツールです。事前のインストールが必要です。

  % gem install bundler

==== 開発サーバーの起動

Rakeタスクで開発サーバーを起動します。

  % bundle exec rake doc:server

doc:server タスクを実行するとWebServerが立ち上がります。((<"http://127.0.0.1:4000/index.html.ja"/"http://127.0.0.1:4000/index.html.ja">)) にアクセスしてください。
Rabbitウェブサイトのホームページが表示されます。
