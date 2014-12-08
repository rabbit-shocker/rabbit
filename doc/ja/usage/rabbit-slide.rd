---
layout: ja
title: rabbit-slideコマンドの使い方
---
== 概要

rabbit-slideはスライドの雛形を生成するコマンドです。

簡単なスライドを作成するにはソースファイルが1つあれば十分ですが、1から
作り始めるよりも雛形があった方が作り始めやすいです。また、スライドを公
開する場合はソースファイル以外にも必要なファイルがあります。それらのス
ライド作成のとっかかりに便利なファイル一式を生成してくれます。

スライドの雛形を生成したら、スライド自体を作成します。スライドの作成時
およびスライド作成後の支援機能も含まれています。支援機能はRakeタスクと
して提供されています。この支援機能を使うと、スライドの表示やPDFの生成、
RubyGems.orgやSlideShareへの公開をコマンド一発で実行できます。

Rabbitでよいスライドを作成し、よい発表をし、そして、共有しましょう！

RubyGems.orgに公開したスライドは((<Rabbit Slide
Show|URL:http://slide.rabbit-shocker.org/>))で閲覧できます。

== 雛形作成

まず、((%rabbit-slide%))コマンドで雛形を生成します。コマンドに新しいス
ライドの情報を渡します。たくさんの情報を渡さないといけないため少し不便
です。今後、GUIで情報を指定できるようにする予定です。

以下はテーマベンチマーク用のスライドの雛形を生成するコマンドです。

  % rabbit-slide new \
     --id theme-benchmark-ja \
     --base-name theme-benchmark \
     --markup-language rd \
     --name "Kouhei Sutou" \
     --email kou@cozmixng.org \
     --rubygems-user kou \
     --tags rabbit

必須のパラメータは((%--id%))と((%--base-name%))です。

((%--id%))はスライドのIDです。ASCII文字のみで指定してください。

((%--base-name%))はスライドのソースファイルの拡張子を除いた部分です。こ
ちらもASCII文字のみで指定してください。

RubyGems.orgで公開する場合は((%--name%))、((%--email%))、
((%--rubygems-user%))などのユーザー情報も指定します。

TODO: すべてのパラメーターの説明を書く。

すべてのパラメーターは((%--help%))で確認できます。

  % rabbit-slide --help
  使い方: rabbit-slide new [オプション]
      例: rabbit-slide new \
            --id rubykaigi2012 \
            --base-name rabbit-introduction \
            --markup-language rd \
            --name "Kouhei Sutou" \
            --email kou@cozmixng.org \
            --rubygems-user kou \
            --slideshare-user kou \
            --speaker-deck-user kou

  スライドの情報
          --id=ID                      スライドID
                                       （例: --id=rubykaigi2012）
                                       （必須）
          --base-name=NAME             スライドのソースファイルとソースから生成するPDFファイルのベースファイル名
                                       （例: --base-name=rabbit-introduction）
                                       （必須）
          --markup-language=LANGUAGE   新しいスライドのマークアップ言語
                                       （例: --markup-language=rd）
                                       （利用可能なマークアップ言語: [rd, hiki, markdown]）
                                       (デフォルト: rd)
                                       （省略可能）
          --title=TITLE                新しいスライドのタイトル
                                       （例: --title="Rabbitの紹介"）
                                       （省略可能）
          --tags=TAG,TAG,...           新しいスライドのタグ
                                       （例: --tags=rabbit,presentation,ruby）
                                       （省略可能）
          --allotted-time=TIME         プレゼンの持ち時間
                                       （例: --allotted-time=5m）
                                       （省略可能）
          --presentation-date=DATE     新しいスライドでプレゼンする日
                                       （例: --presentation-date=2012/06/29）
                                       （省略可能）
  あなたの情報
          --name=NAME                  新しいスライドの作者の名前
                                       （例: --name="Kouhei Sutou"）
                                       (デフォルト: nil)
                                       （省略可能）
          --email=EMAIL                新しいスライドの作者のEメールアドレス
                                       （例: --email=kou@cozmixng.org）
                                       (デフォルト: nil)
                                       （省略可能）
          --rubygems-user=USER         RubyGems.orgのアカウント
                                       RubyGems.orgにスライドを公開するときに使います
                                       （例: --rubygems-user=kou）
                                       (デフォルト: nil)
                                       （省略可能）
          --slideshare-user=USER       SlideShareのアカウント
                                       SlideShareにスライドを公開するときに使います
                                       （例: --slideshare-user=kou）
                                       (デフォルト: nil)
                                       （省略可能）
          --speaker-deck-user=USER     Speaker Deckのアカウント
                                       Speaker Deckにスライドを公開するときに使います
                                       （例: --speaker-deck-user=kou）
                                       （省略可能）

  共通のオプション
          --options-file=FILE          FILEからオプションを読み込みます。
                                       （なし）

          --locale-dir=DIR             ロケールディレクトリを[DIR]にします．
                                       (自動)

          --logger-type=TYPE           ログの出力種類を[TYPE]にします．
                                       [gui, stderr]から選びます．
                                       (STDERR)
          --log-level=LEVEL            ログの出力レベルを[LEVEL]にします．
                                       [debug, info, warning, error, fatal, unknown]から選びます．
                                       (info)

          --help                       このメッセージを表示します．
          --version                    バージョンを表示します．

== 表示

雛形を作成したらスライドIDと同じ名前のディレクトリができているのでそこ
に移動します。ここでは((%--id theme-benchmark-ja%))と指定したとします。

  % cd theme-benchmark-ja

ここで((%rake%))とするとスライドが表示できます。

  % rake

スライドを確認しながらソースファイルを編集してください。ソースファイル
を変更すると自動でスライドの内容が更新されます。

== PDF生成

発表が終わったらPDFでスライドを配布しましょう。配布する前に手元で
PDFを確認したいですよね。((%rake pdf%))でPDFを生成できます。

  % rake pdf

((%pdf/theme-benchmark-ja.pdf%))ができているのでそれをPDFビューアーで開
いてください。なお、((%rabbit%))コマンドもPDFビューアーになるので
((%rabbit%))コマンドでも確認できます。

  % cd pdf
  % rabbit theme-benchmark-ja.pdf

== 公開

現在のところ、RubyGems.orgとSlideShareへ簡単にスライドを公開する機能が
あります。Speaker Deckにも簡単に公開できるようにしようとしましたが、
APIがなかったので諦めました。

公開する場合は雛形作成時に以下のパラメーターを指定しておいてください。

  * ((%--name%))
  * ((%--email%))
  * ((%--rubygems-user%)): RubyGems.orgへ公開する場合
  * ((%--slideshare-user%)): SlideShareへ公開する場合

また、((%README.rd%))の先頭にある以下の部分をスライドにあわせて更新して
ください。


  = TODO: スライドのタイトル

  TODO: スライドの説明

例えば、theme-benchmark-jaの場合は以下のようになっています。

  = テーマベンチマーク

  Rabbitのテーマを確認するためのスライドです。スライドで使われる要素がた
  くさん入っているためテーマの確認に便利です。

準備ができたら((%rabbit publish%))でRubyGems.orgとSlideShareにまとめて
公開できます。別々に公開する方法は以下の通りです。

=== RubyGems.orgへ公開

((%rabbit publish:rubygems%))でRubyGems.orgに公開できます。

  % rake publish:rubygems

RubyGems.orgに公開したスライドは
((%rabbit #{RubyGems.orgのユーザー名}-#{スライドID}.gem%))
で表示できます。theme-benchmark-jaの場合は
RubyGems.orgのユーザー名が((%rabbit%))でスライドIDが
((%theme-benchmark-ja%))なので以下のコマンドで表示できます。

  % rabbit rabbit-theme-benchmark-ja.gem

RubyGems.orgに公開したスライドは((<Rabbit Slide
Show|URL:http://slide.rabbit-shocker.org/>))で閲覧できます。URLは
((%http://slide.rabbit-shocker.org/authors/#{RubyGems.orgのユーザー
名}/#{スライドID}/%))です。

Rabbit Slide ShowはRubyGems.orgに公開されているスライドを自動で収集して
表示しているので、明示的にRabbit Slide Showにスライドを公開する操作を
する必要はありません。RubyGems.orgに公開するだけでOKです。

=== SlideShareへ公開

((%rabbit publish:slideshare%))でSlideShareに公開できます。

  % rake publish:slideshare

無事に公開できたら自動でスライドページを開きます。
