---
layout: ja
title: rabbit-themeコマンドの使い方
---
== 概要

rabbit-themeはテーマの雛形を生成するコマンドです。

簡単なテーマを作成するにはテーマファイルが1つあれば十分ですが、1から作
り始めるよりも雛形があった方が作り始めやすいです。また、スライドを公開
する場合はテーマファイル以外にも必要なファイルがあります。それらのテー
マ作成のとっかかりに便利なファイル一式を生成してくれます。

テーマの雛形を生成したら、テーマ自体を作成します。テーマの作成時および
テーマ作成後の支援機能も含まれています。支援機能はRakeタスクとして提供
されています。この支援機能を使うと、テーマ確認用スライドの表示やPDFの生
成、RubyGems.orgへの公開をコマンド一発で実行できます。

Rabbitでよいテーマを作成し、よい発表をし、そして、共有しましょう！

TODO: RubyGems.orgに公開したスライドは
((<URL:http://theme.rabbit-shocker.org/>))で閲覧できるようになる予定で
す。

== 雛形作成

まず、((%rabbit-theme%))コマンドで雛形を生成します。コマンドに新しいテー
マの情報を渡します。今後、GUIで情報を指定できるようにする予定です。

以下は札幌Ruby会議2012用のテーマの雛形を生成するコマンドです。

  % rabbit-theme new \
     --id sprk2012 \
     --name "Kouhei Sutou" \
     --email kou@cozmixng.org \
     --rubygems-user kou

必須のパラメータは((%--id%))です。

((%--id%))はテーマのIDです。ASCII文字のみで指定してください。

RubyGems.orgで公開する場合は((%--name%))、((%--email%))、
((%--rubygems-user%))でユーザー情報も指定します。

すべてのパラメーターは((%--help%))で確認できます。

  % rabbit-theme --help
  使い方: rabbit-theme new [options]
      例: rabbit-theme new \
            --id rubykaigi2012 \
            --name "Kouhei Sutou" \
            --email kou@cozmixng.org \
            --rubygems-user kou

  テーマの情報
          --id=ID                      テーマID
                                       （例: --id=rubykaigi2012）
                                       （必須）
  あなたの情報
          --name=NAME                  新しいスライドの作者の名前
                                       （例: --name="Kouhei Sutou"）
                                       (デフォルト: Rabbit)
                                       （省略可能）
          --email=EMAIL                新しいスライドの作者のEメールアドレス
                                       （例: --email=kou@cozmixng.org）
                                       (デフォルト: lavie@rabbit-shocker.org)
                                       （省略可能）
          --rubygems-user=USER         RubyGems.orgのアカウント
                                       RubyGems.orgにスライドを公開するときに使います
                                       （例: --rubygems-user=kou）
                                       (デフォルト: rabbit)
                                       （省略可能）

  共通のオプション
          --options-file=FILE          FILEからオプションを読み込みます。
                                       （なし）

          --locale-dir=DIR             ロケールディレクトリを[DIR]にします。
                                       (自動)

          --logger-type=TYPE           ログの出力種類を[TYPE]にします。
                                       [gui, stderr]から選びます。
                                       (STDERR)
          --log-level=LEVEL            ログの出力レベルを[LEVEL]にします。
                                       [debug, info, warning, error, fatal, unknown]から選びます。
                                       (info)

          --help                       このメッセージを表示します。
          --version                    バージョンを表示します。

== 表示

雛形を作成したらテーマIDと同じ名前のディレクトリができているのでそこに
移動します。ここでは((%--id sprk2012%))と指定したとします。

  % cd sprk2012

ここで((%rake%))とするとテーマ確認用のスライドが表示できます。

  % rake

スライドを確認しながらテーマを編集してください。テーマ変更後、スライド
上で((%r%))を押すとテーマが再読み込みできます。

== PDF生成

画面に表示するときだけではなく、PDFにしたときにどうなるかも気になります
よね。((%rake pdf%))でPDFを生成できます。

  % rake pdf

((%pdf/sprk2012.pdf%))ができているのでそれをPDFビューアーで開いてくださ
い。なお、((%rabbit%))コマンドもPDFビューアーになるので((%rabbit%))コマ
ンドでも確認できます。

  % rabbit pdf/heme-benchmark-ja.pdf

== RubyGems.orgに公開

RubyGems.orgに公開する場合は雛形作成時に以下のパラメーターを指定してお
いてください。

  * ((%--name%))
  * ((%--email%))
  * ((%--rubygems-user%)): RubyGems.orgへ公開する場合
  * ((%--slideshare-user%)): SlideShareへ公開する場合

また、((%README.rd%))の先頭にある以下の部分をスライドにあわせて更新して
ください。


  = TODO: テーマのタイトル

  TODO: テーマの説明

例えば、sprk2012の場合は以下のようになります。

  = 札幌Ruby会議2012

  札幌Ruby会議2012用のテーマ。札幌Ruby会議2012公式壁紙を背景にしていま
  す。

準備ができたら((%rabbit publish%))でRubyGems.orgに公開します。

  % rake publish

RubyGems.orgに公開したテーマは
((%rabbit -t #{テーマID} rabbit-theme-benchmark-ja.gem%))
で表示できます。札幌Ruby会議2012用のテーマはテーマIDが
((%sprk2012%))なので以下のコマンドで表示できます。

  % rabbit -t sprk2012 rabbit-theme-benchmark-ja.gem
