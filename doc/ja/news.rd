---
layout: ja
title: お知らせ
apply_data: false
---
== 2.0.2: 2012-09-02

バグフィックスリリース。

=== 修正

==== rabbit

  * gemのテーマを検索しない問題を修正。

==== テーマ

  * nari: 見出しが不用意にセンタリングされることがある問題を修正。

== 2.0.1: 2012-08-31

バグフィックスリリース。

=== 改良

==== rabbit

  * .rabbitで指定したスライドソースよりもコマンドラインで指定したスライ
    ドソースの方を優先して使うようにした。

=== 修正

==== rabbit

  * 情報ウィンドウが動作しない問題を修正。 [nariさんが修正]
  * インデックスモードが動作しない問題を修正。 [畠山さんが報告]
  * TeXフォーマット機能が動作しない問題を修正。 [畠山さんが報告]

=== 感謝

  * nariさん
  * 畠山さん

== 2.0.0: 2012-08-29

約一年ぶりのメジャーバージョンアップ！

スライドとテーマを共有できるようになりました。スライドもテーマも
RubyGems.orgにgemとして登録しておきます。スライドgemを指定すると自動で
インストールして表示します。gemで公開されているテーマを指定したときも自
動でインストールしてそのテーマを使います。

また、コマンド一発でスライドをSlideShareに公開するユーティリティも追加
しました。よりスライドを共有しやすくなっています。

また、RabbiRackやRabbiterといったRabbitを便利にするツールを別パッケージ
にしました。これまでは、これらのツールを使うために別途依存ライブラリを
手動でインストールする必要がありましたが、別パッケージにしたことにより、
それらを自動化できるようになりました。これでインストールが楽になってい
ます。

=== 改良

==== rabbit

  * gettext gemを必須ライブラリにした。
  * rttool gemを必須ライブラリにした。
  * URL指定のスライド中で使っている相対パスの画像を扱えるようにした。
  * カレントディレクトリに.rabbitファイルがある場合は、そのファイルから
    コマンドライン引数を読むようにした。
  * スライドとして.rabbitファイルがあるディレクトリを指定できるようにした。
  * gemで公開されたスライドを直接表示できるようにした。
  * gemで公開されたテーマを自動でインストールして使うようにした。
  * ファイルに書かれたコマンドライン引数を指定する--options-fileオプショ
    ンを追加。
  * setup.rbを削除。
  * 非推奨になっていた--druby-uriオプションを削除。
  * インストール時にデフォルト画像ディレクトリのパスをカスタマイズする
    機能を削除。不必要になったため。
  * テーマ用のRubyスクリプトファイル名として(({theme.rb}))を使うように
    した。これまで通り(({#{テーマ名}.rb}))も使えるが非推奨。
  * テーマ名として(({.}))を指定した場合はカレントディレクトリのテーマを
    使うようにした。

==== rabbit-slide

新規追加した機能。以下のようなスライドに関する便利機能を提供する。

  * 新規スライドの雛形を生成機能。
  * スライドをRubyGems.orgにアップロードする機能。
  * SlideShareにアップロードする機能。
  * 簡単にスライドを表示する機能。
  * 簡単にPDFを生成する機能。

==== rabbit-theme

新規追加した機能。以下のようなテーマに関する便利機能を提供する。

  * 新規テーマの雛形を生成機能。
  * テーマをRubyGems.orgにアップロードする機能。
  * 簡単にテーマ確認用スライドでテーマを確認する機能。
  * 簡単にPDFを生成する機能。

==== テーマ

  * background-image-toolkit: 背景画像をURLで指定できるようにした。
    [佐々木さんが提案]
  * image-slide-number: デフォルトで旗にページ番号を表示するようにし
    た。
  * default: デフォルトでうさぎとかめを有効にした。

=== 修正

==== ドキュメント

  * 数値文字参照の間違った説明を修正。[znzさんが報告]

=== 変更

  * RabbiRackを別パッケージ（rabbirack gem）にした。
  * Rabbiterを別パッケージ（rabbiter gem）にした。
  * RabWiiを別パッケージ（rabwii gem）にした。
  * テーマベンチマーク用のスライドを別パッケージ
    （rabbit-slide-rabbit-theme-benchmark-ja）にした。

=== 感謝

  * znzさん
  * 佐々木さん

== 1.0.9: 2012-07-21

1.0.8のバグフィックスリリース

=== 改良

==== rabbit-mode.el

  * 挿入系のコマンドはprefixとして((%C-cC-i%))を使うように変更。

==== rabbit

  * RDとHikiで数値文字参照をサポート:

    RD:
      (('&#x1D11E;')) （16進数）
      (('&#119070;')) （10進数）

    Hiki:
      {{code_point(0x1d11e)}} （16進数）
      {{code_point(119070)}} （10進数）

==== rabbirack

  * 不必要なエンコーディングの変更をやめた。

==== rabbiter

  * twitter-stream 0.1.16対応。それ以前のバージョンはサポート対象外。
  * Ruby/GIO2を使って通信するようにした。

== 1.0.8: 2012-06-17

1.0.7のバグフィックスリリース

=== 修正

==== rabbit

  * 画像を表示できない問題を修正。 [TAKATSU Tomonariさんが報告]

=== 感謝

  * TAKATSU Tomonariさん

== 1.0.7: 2012-05-29

twitter-stream 0.1.15対応リリース

=== 改良

==== rabbit

  * 拡張子がmdのファイルだけではなく、markdownのファイルもMarkdown記法
    の入力ファイルとして処理するようにした。
  * Markdownでリンクのマークアップに対応した。 [北市真さんがパッチ提供]

==== テーマ

  * default-preformatted: @preformatted_centering変数で整形済みテキスト
    をセンタリングするかどうかをカスタマイズできるようにした。
    [kimura wataruさんがパッチ提供]
    * (({true})): センタリングする（デフォルト）
    * (({false})): 左寄せ

==== rabbiter

  * twitter-stream 0.1.15に対応。

=== 修正

==== rabbit

  * スペースのサイズを間違って計算していた問題を修正。

==== テーマ

  * 中央揃えがリセットされない問題を修正。

==== ドキュメント

  * typoを修正 [とみたまさひろさんが報告]

=== 感謝

  * 北市真さん
  * kimura wataruさん
  * とみたまさひろさん

== 1.0.6: 2012-03-03

CodeRay 1.0.x対応リリース

=== 改良

==== rabbit

  * CodeRay 1.0.xに対応。 [TAKATSU Tomonariさんが報告]

=== 感謝

  * TAKATSU Tomonariさん

== 1.0.5: 2012-01-30

Markdown対応リリース！

=== 改良

==== rabbit

  * ノートモードを追加。 [nariさんが追加]

==== rabbirack

  * wait対応。

==== レンダリング

  * テキストレンダリング速度を改良。（特にCodeRayを使った場合）
  * SVG: 鏡面反射をサポート。

==== マークアップ

  * マークアップの検出時に拡張子も参照することで検出精度を向上。
  * RD: "block-quote"というキーワードでも"block_quote"という
    キーワードでも引用扱いになるようにした。
  * kramdownを使ってMarkdownサポートを追加。

==== テーマ

  * syntax-highlighting: デフォルトの前景色を設定した。
  * nari: headline-alignに対応。
  * tag: タグ毎の処理をカスタマイズできるようにした。
  * slide-show: waitに対応。
  * slide-footer-info: @slide_footer_info_line_colorだけ設定
    しても有効になるようにした。
    [kimura wataruさんが提案]

=== 修正

==== rabbit

  * [GitHub#4]: warningメソッドがなかったのを追加。
    [TAKATSU Tomonariさんが修正]

==== rabbirack

  * [GitHub#5]: Rack 1.2.1対応。
    [TAKATSU Tomonariさんが報告]

==== rabbit-command

  * [GitHub#3]: 変数名が間違っていた問題を修正。
    [TAKATSU Tomonariさんが修正]

==== rabbitter

  * HTTPS対応。 [OBATA Akioさんが修正]

==== テーマ

  * [GitHub#2]: default-slide: headline-alignスライドプロパ
    ティが動かなくなっていた問題を修正。 [nariさんが報告]

==== ドキュメント

  * 英語用MLのアドレスを修正。

=== 感謝

  * nariさん
  * TAKATSU Tomonariさん
  * OBATA Akioさん
  * kimura wataruさん

== 1.0.4: 2011-08-06

テーマ改良リリース！

=== 改良

==== テーマ

  * nari: 追加。nariさん用テーマ。[nariさんが作成]
  * image-slide-number: デフォルトのうさぎをかわいいうさぎに変更。
  * image-slide-number: 旗の色をカスタマイズできるようにした。
  * image-timer: デフォルトのかめをかわいいかめに変更。
  * rubykaigi2011: うさぎとかめに対応。

=== 修正

==== ドキュメント

  * Pangoのリンクを修正。[無量井さんが修正]

=== 感謝

  * nariさん
  * 無量井さん

== 1.0.3: 2011-07-17

1.0.2のバグフィックスバージョン。

=== 改良

==== 本体

  * RubyGems 1.8.5対応。
    [okkezさんが報告]
  * rabbitコマンドにファイル名を指定しなかった場合は標準入力
    を使うのではなく、常にファイル選択ダイアログを開くように
    した。

=== 修正

==== 本体

  * Wiki記法を使えないのに使えるようにみえてしまう問題を修正。
    [木村さんが報告]
  * Sinatraへの依存関係が抜けていた問題を修正。

==== テーマ

  * footer-comment: コメントがすぐに消えてしまう問題を修正。

=== 感謝

  * okkezさん
  * 木村さん

== 1.0.2: 2011-07-15

日本Ruby会議2011リリース。

=== 改良

==== 本体

  * GTK+ 2.20を再サポート。（Debian GNU/Linux squeezeのため）
    [やまだあきらさんが報告]

==== テーマ

  * title-slide-background-image: 追加。タイトルスライドのス
    ライドいっぱいに背景画像を表示する。
  * table: 文字の色のカスタマイズに対応。
  * rubykaigi2011: 追加。日本Ruby会議2011用テーマ。

=== 修正

==== テーマ

  * default-item-mark-setup: フォント名の重複指定エラーを修正。
  * edge-info-toolkit: フォント名の重複指定エラーを修正。

=== 感謝

  * やまだあきらさん

== 1.0.1: 2011-07-15

1.0.0のバグフィックスバージョン。

=== 修正

==== 本体

  * 不必要なalbino gemへの依存をなくした。

== 1.0.0: 2011-07-15

7年目にして初のメジャーリリース！

=== 改良

==== 本体

  * Twitter関連のgemの依存関係を必須ではなくオプションにした。
  * 明示的に--use-glをしない限りClutterを使わないようにした。
    [おばたさんが報告]
  * ((<blockdiag|URL:http://blockdiag.com/blockdiag-ja/build/html/>))対応。

    RD:
      # blockdiag
      # relative_width = 90
      # fonts = /usr/share/fonts/opentype/ipafont/ipag.ttf, /usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf
      {
        fontsize = 25;
        RD -> Rabbit;
        Hiki -> Rabbit;
        PDF -> Rabbit;
        group {
          Rabbit -> Display;
        }
      }

    Hiki:
      {{blockdiag("
      {
        fontsize = 25;
        RD -> Rabbit;
        Hiki -> Rabbit;
        PDF -> Rabbit;
        group {
          Rabbit -> Display;
        }
      }",
                  {
                    :relative_width => 90,
      #             :antialias => true,
                    :fonts => ["/usr/share/fonts/opentype/ipafont/ipag.ttf",
                               "/usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf"],
                  })}}
  * Anthy対応を削除。
  * CodeRayによるシンタックスハイライト対応。
  * Emacsを用いたシンタックスハイライト対応。
  * コンソールからの起動の検出処理の改良。
    [おばたさんが報告]
  * Twitterとの通信をnon-blockingモードにできなくてもエラー
    にせずに警告にするようにした。
    [おばたさんが報告]
  * 不必要なGUIでのログ出力を抑制。
    [おばたさんが報告]
  * HTreeではなくNokogiriを使うようにした。
  * プレゼン時間を指定する--allotted-timeオプションを追加。
  * コメント用のテーマを指定する--comment-themeオプションを
    追加。
  * RWiki対応を削除。

==== Rabbiter

  * デバッグ用に取得した情報を出力する--log-statusオプション
    の追加。

==== Rabbirack

  * Rabrickに変わるRabbitのWebインターフェイス。

==== テーマ

  * テキスト要素にshadow-color, shadow-x, shadow-yプロパティ
    を追加。
  * syntax-highlighting: 追加。シンタックスハイライトの色を
    指定するテーマ。
  * footer-comment:
    * image-timerと一緒に使ったときでも見やすくした。
    * デフォルトでは最後のコメントは残さないようにした。
  * rabbit-powered-by: バナー画像を表示しないようにした。
  * pdf-tortoise-and-hare: pdfテーマに統合したため削除。
  * pdf:
    * プレゼン時間が指定されていたら自動でうさぎとかめを
      表示するようにした。
    * コメント表示に対応。
  * base: 前景色・背景色・影の色を指定するようにした。
  * default-comment: 追加。コメント用テーマを指定するテーマ。

=== 修正

==== 本体

  * パッケージに.moが含まれていない問題を修正。
    [おばたさんが報告]
  * rabbiterからのコメント追加時にクラッシュする問題を修正。
  * Ruby 1.9でPDFファイルの自動検出に失敗する問題を修正。

==== ドキュメント

  * リンク先を修正。
    [znzさんが報告]

=== 感謝

  * おばたさん
  * znzさん

== 0.9.3: 2011-06-25

コメント・Twitterサポート強化リリース。

=== 改良

==== 本体

  * PDFサポートのRuby 1.9対応。
    [とみたさんが報告]
  * [#199] "-I"オプションで指定されたパスの中の「~」を展開す
    るようにした。
    [kdmsnrさんが報告]
  * TwitterのOAuthに対応。
  * Bundlerに対応。
  * rabbitコマンド終了時に実行する処理を登録できる
    Rabbit.add_cleanup_procを追加。
  * Wii RemoteでRabbitを操作するためのコマンドrabwii（らぶ
    うぃー）を追加。
    [おかべさんのコードをベースに改造]
  * dRubyサーバはデフォルトでlocalhostにバインドするようにし
    た。
  * デフォルトでRabbit APIをすべて公開するようにした。
    （--default-public-level=all）
  * rabbiter: OAuthを使用するようにした。
  * rabrick: --druby-uriオプションを--rabbit-uriに変更した。
  * rabbit-command: --druby-uriオプションを--rabbit-uriに変更した。
  * コメントビューの削除。
  * rabbiter: --user-languageオプションを追加。
  * Windowsではrubyw.exeで起動されていたらデフォルトでGUIロガー
    を使うようにした。
    [なかださんが手助け]
  * コンソールなしの状態で引数を指定せずに起動した場合はファ
    イル選択ダイアログを表示するランチャーモードを追加。
  * Hiki記法でタグをサポート。（{{tag('タグ名', '値')}}）

==== テーマ

  * title-background-image: スライドプロパティでの画像ファイ
    ル名指定に対応。
    [kdmsnrさんが提案]
  * background-image-toolkit: テーマでオプションが指定されて
    いても常にスライドプロパティを使うようにした。
  * image: 画像のキャプションのフォントサイズをカスタマイズ
    できるようにした。
  * color-circle: newline-in-slidesテーマとtagテーマを有効に
    した。
  * slide-number: スライド番号を表示する位置をカスタマイズで
    きるようにした。
    [佐々木さん]
  * debian: アップデート。
    [佐々木さん]
  * dark-gradation: 追加。Keynote風の暗めのグラデーションの
    テーマ。
    [佐々木さん]
  * twitter-comment: 追加。Twitterのtweetをコメントとして取
    り込むテーマ。
    [おかべさんのコードをベースに改造]
  * comment: 削除。廃止されたコメントビュー用のテーマ。
  * footer-comment: 追加。コメントをフッターに表示するテーマ。
    [おかべさんのコードをベースに改造]
  * clutter-comment: 追加。コメントをClutterのAPIを利用して
    ぐりんぐりんさせながら表示するテーマ。
  * stream-comment: 追加。コメントをスライド上に流すテーマ。

=== 修正

==== 本体

  * Clutterバックエンドが動かない問題を修正。
    [おばたさんが報告]
  * GUIロガーが動かない問題を修正。

=== 感謝

  * とみたさん
  * kdmsnrさん
  * おばたさん
  * 佐々木さん
  * おかべさん
  * なかださん

== 0.9.2: 2010-12-31

Matz向けリリース。

=== 改良

==== テーマ

  * Debianテーマを追加。[佐々木さんが追加]
  * cairoバックエンド:
    * line_cap対応。
    * line_join対応。
    * パターンのソースとしてPixbufに対応。
    * パターンの行列変換に対応。
  * default-slide: 線幅のカスタマイズに対応。
    * @default_headline_line_width
    * @default_headline_line_params
    * @default_headline_line_expand
  * tag:
    * margin-topタグの追加。
    * margin-bottomタグの追加。
  * slide-logo: スライドロゴの幅・高さ・位置のカスタマイズに対応。
    * @slide_logo_width
    * @slide_logo_height
    * @slide_logo_position
  * ラングバテーマを追加。

==== 情報ウィンドウ

  * タイマー開始前でも残り時間を表示。 [まつもとさんが提案]
  * 現在のスライドも表示。 [まつもとさんが提案]
  * 情報ウィンドウからキーボード・マウスでのスライド操作に対応。
    [まつもとさんが提案]

=== 感謝

  * 佐々木さん
  * まつもとさん

== 0.9.0から0.9.1の変更点: 2010-10-25

ライセンスの変更: Rubyライセンス → GPLv2 or later

=== 改良

  * gemからrabbit.batを削除。[うささん]
  * Homebrew用のドキュメントを追加。[角さん]
  * ライセンスをRubyライセンスからGPLv2 or laterに変更。
    （Rabbitに貢献してくれたコードなどのライセンスは須藤功平
    が自由に変更できるものとします。これに同意してもらえない
    場合は受けとりません。）

=== 感謝

  * うささん
  * 角さん

== 0.6.5から0.9.0の変更点: 2010-09-26

カスタムタグのデフォルトスタイルを適用。

=== 改良

==== テーマ

  * background-image-toolkit: 縦方向の位置を設定する
    vertical-alignパラメータの追加。

    例:
      # image
      # src = lavie.png
      # relative-width = 30
      # align = right
      # vertical-align = top
      # relative-margin-right = -5

  * tag: 新規。カスタムタグ用のデフォルトスタイルを指定する
    テーマ。以下のカスタムタグをサポート。デフォルトで有効。

    * tag:x-large: 指定したテキストの文字を大きくする。
    * tag:center: 指定したテキストを中央寄せして配置する。テ
      キストを指定しない場合はそのブロック全体を中央寄せする。
    * tag:center: 指定したテキストを右寄せして配置する。テ
      キストを指定しない場合はそのブロック全体を右寄せする。

    例:
      (('tag:x-large:文字の大きなテキスト'))

      (('tag:center'))中央寄せされたテキスト

      (('tag:right'))右寄せされたテキスト

==== RD

  * (('('))('XXX:')((')'))形式のマークアップ内での再帰的なマー
    クアップのサポート。

  * テーブルのセル内でのマークアップをサポート。

=== 修正

  * require忘れを修正。
    [おばたさんが報告]
  * aafigureのサイトのURLを修正
    [kdmsnrさんが報告]
  * background-image-toolkitテーマのデフォルトの配置位置を
    後方互換性のためcenterに変更。
    [nariさんが提案]
  * RabbitterをRabbiterに変更。

=== 感謝

  * おばたさん
  * kdmsnrさん
  * nariさん

== 0.6.4から0.6.5の変更点: 2010-07-31

  * Twitterからコメントを拾ってくるRabbitterを追加しました。
    [おばたさん]
  * ((<aafigure|URL:https://launchpad.net/aafigure>))
    に対応しました。 [kdmsnrさんが提案]

=== 改良

  * デフォルトのPDFのファイル名から不要な改行を削除。
  * 改行文字が\r\nでも動作するようにした。
    [zundaさんが報告]
  * Rabbitter: 追加。Twitterからコメントを拾ってくる。
    [おばたさん]
  * aafigure対応。[kdmsnrさんが提案]

==== テーマ

  * clear-blue: フッターのテキストから不要な改行を削除。
  * per-slide-background-iamge:
    "background-image-align: right"スライドプロパティ対応。
    [kdmsnrさんが提案]

      = スライド

      ...

      == プロパティ

       : background-image
          lavie.png

       : background-image-relative-width
          30

       : background-image-align
          right

       : background-image-relative-margin-right
          3

  * relative_widthでもrelative-widthでも画像の幅を指定できる
    ようにした。

  * body-background-image: 追加。"align = right"画像プロパティ
    が使えるようになる。

      = タイトル

        # image
        # src = lavie.png
        # relative-width = 30
        # align = right
        # relative-margin-right = -5

  * デフォルトでbody-background-imageと
    per-slide-background-imageを有効にした。

  * background-image-tookit: 追加。body-background-imageと
    per-slide-background-imageテーマの共通部分を汎用化した
    ツールキット。

  * title-background-image:
    @title_background_image_propertiesによるカスタマイズに対
    応。画像要素に使えるオプションと同じものが指定できる。
    [kdmsnrさんが提案]

      @title_background_image_properties = {
        :align => :right,
        :as_large_as_possible => false,
        :relative_height => 75,
      }

    上記のオプションでは以下のようになる。
        +-----------+
        |      +---+|
        |  タイトル <- 背景画像
        |      +---+|
        +-----------+

    デフォルトでは、背景画像はできるだけ拡大して、中央に配置
    されます。

=== 修正

  * コメント表示が動かない問題を修正

=== 感謝

  * kdmsnrさん
  * zundaさん
  * おばたさん

== 0.6.3から0.6.4の変更点: 2010-01-29

Debianオフィシャルパッケージになりました！[佐々木さん]

=== 改良

  * 常にウィンドウを最全面に表示する--keep-aboveオプションの追加。
  * HTML出力時にソースへのリンクを追加する--source-filename
    オプションの追加。
  * ソースと同じディレクトリがテーマの検索パスに含まれるよう
    になった。
  * ソースと同じディレクトリにテーマを置けるようになった。
    [zundaさん]
  * PDFファイルの検出率向上。

==== テーマ

  * 追加
    * per-slide-background-image:
      スライド毎に背景画像を指定できるようになります。画像は
      スライドプロパティで指定します。

        = 対象となるスライド

        ...

        == プロパティ

        : background-image
           my-picture.png
        : background-image-ralative-height
           95

      スライド内で画像を指定する時と同じオプションを指定でき
      ます。オプションを指定する場合はプロパティ名を
      「background-image-」からはじめてください。

      例えば、「relative-height」オプションを指定する場合は
      「background-image-relative-height」スライドオプション
      になります。

    * per-slide-background-color:
      スライド毎に背景色を指定できるようになります。色は
      スライドプロパティで指定します。

        = 対象となるスライド

        ...

        == プロパティ

        : background-color
           black

      色はテーマ内での指定方法と同様に「black」など色の名前
      でも「#RRGGBBAA」というようにRGB値でも指定できます。

  * 変更
    * default-slide:
      スライドプロパティでスライド毎にタイトルの前景色と影の
      色を変更できるようになりました。

        = 対象となるスライド

        ...

        == プロパティ

        : headline-color
           red
        : headline-shadow-color
           gray

=== 修正

  * タイトルがないときに
    @lightning_talk_as_large_as_possible = trueを指定すると
    落ちる問題を修正。[kdmsnrさん]
  * Ruby 1.8.7 p249で動かない問題を修正。

=== 感謝

  * zundaさん
  * kdmsnrさん
  * 佐々木さん

== 0.6.2から0.6.3の変更点: 2009-12-16

=== 改良

  * [#180] Wikiフォーマットの定義リスト内でインラインマーク
    アップが効かない問題を修正。
    [kdmsnrさん]
  * 執事うさぎを標準テーマに追加。
  * 印刷時でもデフォルトでは拡大・縮小後の画像を使用。
  * アルファチャンネル付きのpixbufレンダリング対応。
    （cairoレンダリングエンジン使用時）

==== テーマ

  * 追加
    * lightning-monochrome:
      高橋メソッドスライド + モノクロな通常スライド
  * 変更
    * default-block-quote: 背景画像の透明度を指定する
      @block_quote_image_background_alphaパラメーターを追加。
    * default-preformatted: テキストの自動サイズ調整をカスタ
      マイズできるようにした。@preformatted_keep_in_sizeパラ
      メータで変更可能。
    * clear-blue: デフォルトで左下にタイトルを表示するように
      変更。無効にする場合は以下を追加するとよい。
        include_theme("clear-blue")
        @slide_footer_info_left_text = ""
  * 箇条書きと列挙のネスト"itemize > enum"と
    "itemize > itemize > enum"に対応

==== 実験的

((*変更される可能性があります。*))

  * タグのサポート

    構文:
      (('tag:name:内容'))
    または
      (('tag:name'))内容

    例:
      slide.rab:
        (('tag:center'))どうもどうも

      theme.rb:
        match("**", CustomTag) do |tags|
          tags.each do |tag|
            case tag.name
            when "center"
              tag.parent.horizontal_centering = true
            end
          end
        end

    出力（テーマ適用前）:
      +-------------------+
      | どうもどうも      |
      +-------------------+

    出力（テーマ適用後）:
      +-------------------+
      |   どうもどうも    |
      +-------------------+

=== 修正

  * Ruby 1.9でフルスクリーンが動かない問題を修正 [助田さん]
  * [#179] マウス使用時にクラッシュすることがある問題を修正
    [kdmsnrさん]
  * newline-in-slidesテーマ: 整形済みテキストでは改行を置換
    しないようにした。
  * フルスクリーン解除時にカーソルが表示されない問題を修正

=== 感謝

  * 助田さん
  * kdmsnrさん

== 0.6.1から0.6.2の変更点: 2009-10-03

=== 改良

  * RabbitがMacPortsの正式パッケージになった！ [木村さん]
  * Wikiの'''すごい強調'''マークアップに対応 [kdmsnrさん]
  * Wikiマークアップでレベル2以上のセクションを無視
    [kdmsnrさん]
  * テーマ
    * pdf-tortoise-and-hare: 画面端とうさぎとかめの間にマー
      ジンを設定
    * clear-blue: うさぎとかめの画像が変更可能に。
    * set_font_resolution_ratioの追加。文字の大きさを一括で
      変更できる。
  * rabbit-mode: rabbit-commandに対応。
  * prototype.jsをアップデート: 1.4.0 -> 1.6.0
  * 両端揃えに対応。
  * 現在のスライドをRDフォーマットで取得するコマンドを追加。
  * --geometryオプションの追加。
  * RubyGemsに対応。

=== 修正

  * lightning-simpleテーマでallotted-timeの設定が無視される
    問題を修正。 [西山さん]
  * システムのDPIによって文字の大きさが変わってしまう問題を
    修正。 [西山さん]
  * 画面とPDFで文字の大きさが変わってしまう問題を修正。
    [西山さん]
  * GDKバックエンドで画像がリサイズされない問題を修正。

== 0.6.0から0.6.1の変更点: 2009-07-17

=== 改良

  * 画像をディスプレイ表示時はGdk::Pixbufで拡大縮小してから
    描画
  * テーマ
    * lightning-talk-toolkit: :hide-titleをサポート
    * slide-show: ループ毎にタイマーをリセット
    * slide-show: 持ち時間からデフォルトのスライド切り替え時
      間を算出
  * デフォルトで"\n"を使用可能に変更
  * Wikiフォーマットがスライドプロパティに対応
  * RDフォーマットがネストした箇条書きの'wait'に対応 [西山さん]

=== 修正

  * GPLのライセンス文を追加
  * 画面サイズが変わったときにPDFのサイズが拡大縮小されない
    問題を修正
  * タイトルページしかない場合にインデックスモードに変更でき
    ない問題の修正 [rabbit-shockers:654] [おばたさん]
  * ブロックレベルでのwaitが動かない問題の修正 [おばたさん]

== 0.5.9から0.6.0の変更点: 2009-05-23

=== 改良

  * Bonjour対応: [木村さん]
  * マスコットキャラクタ「たいらび」の追加: [モモさん]
    * ((<URL:http://www.cozmixng.org/repos/rabbit/trunk/sample/momo/tailavi/>))
  * ウィンドウタイトルにサブタイトルも表示
  * スライドプロパティ対応
  * 「うさぎとかめ」が使えないような場でのプレゼン用の代替画像を追加
  * PDFに埋め込む画像をリサイズ前の元画像を埋め込むように変更

=== 修正

  * sample/kof2005/gesture.rbの修正: [#143][いわいさん]
  * インストールドキュメントの修正: [zundaさん]
  * 英語の修正: [Eduardoさん]

== 0.5.8から0.5.9の変更点: 2009-02-09

  * 改良
    * HTML出力: PDFへのリンク対応
    * Rabbitをコマンドラインから制御するrabbit-commandの追加
    * 鏡に反射したような画像効果のサポート
      （kdmsnrさんからのリクエスト）

      使用例:
        * ((<URL:http://www.clear-code.com/archives/SendaiRubyKaigi01/love-and-continue-it-104.html>))
        * ((<URL:http://www.clear-code.com/archives/SendaiRubyKaigi01/love-and-continue-it-085.html>))

      書き方:
        # image
        # src = XXX.jpg
        # relative_height = 80
        # reflect_ratio = 0.5

  * テーマ
    * 新規テーマ
      * pdf-tortoise-and-hare:
        PDFビューアとしてRabbitを使うときに、うさぎとかめタ
        イマーを使う。持ち時間はRABBIT_ALLOTTED_TIME環境変数
        で指定。

        使い方:
          % RABBIT_ALLOTTED_TIME=4.5m rabbit --theme pdf-tortoise-and-hare XXX.pdf
      * lightning-clear-blue:
        青っぽいclear-blueテーマの中でたまに大きな文字のスラ
        イドを使う。
      * title-on-image-toolkit:
        画像の上にタイトルを重ねる。

        使用例:
        ((<URL:http://www.clear-code.com/archives/SendaiRubyKaigi01/love-and-continue-it-036.html>))

        使い方:
          スライド:
            = ユーザーズグループ

              # image
              # src = shocker.jpg
              # relative_height = 90

          テーマ:
            include_theme("title-on-image-toolkit")

            match(Slide) do |slides|
              slides.each do |slide|
                slide.title_on_image if slide.title_on_image?
              end
            end

  * バグ修正
    * Wikiフォーマットの検出を失敗する問題の修正

== 0.5.7から0.5.8の変更点: 2008-10-19

  * 改良
    * Clutterのサポート
      * ページ切り替え時の効果をサポート
      * スライド上に流れるコメント表示方法をサポート
    * IRCバックエンドのサポート
      * RabbIRC（やまださん）
  * テーマ
    * 引用マークに影を追加
    * ヒラギノフォントを優先的に使うように変更
    * 新規テーマ
      * newline-in-slides: \nで改行を入力
  * バグ修正
    * 虫眼鏡でマウスポインタを掴みつづける問題の修正
    * Tofuバックエンドが動かなくなっている問題の修正
    * Asakusa.rbで報告されていた問題の修正

== 0.5.6から0.5.7の変更点: 2008-07-31

  * バグの修正
    * 壊れたPDFを出力問題の修正
  * 改良
    * GTK+/Quartz対応（Mac OS Xネイティブで動く）
    * Control + 左クリックでコンテキストメニュー（おばたさんに
      よる提案）
    * align = XXXで:rightや"left"を指定可能にした
    * (('note:XXX:YYY:...'))というようにラベルを入れ子にでき
      るようにした
    * 「次」・「前」アクション（ポーズを考慮した移動アクショ
      ン）の追加
    * 「タイマーリセット」アクションの追加
    * コマンドラインでフルスクリーンが指定された場合はできる
      だけ早くフルスクリーンにするようにした
    * Wiki形式で{{wait}}と{{br}}を使えるようにした
    * サンプルのスライドが汚かったのをいいかげんに直した
  * テーマ
    * 本体部分の余白を増やした
    * 項目マークの種類に"dash"を追加
    * デフォルトテーマのリストマークでMagicPoint由来のド派手
      な色を使うことをやめた
    * ひととおりプロパティを網羅
    * 新規テーマ
      * newline-in-title: \nで改行を入力
      * edge-info-toolkit: スライドの端に何かを表示
      * slide-header-info: スライドの上部に何かを表示
      * slide-footer-info: スライドの下部に何かを表示
      * footer-logo: スライドの下部にロゴを表示
      * blue-bar: スライドの上下に青いバーがあるテーマ
      * clear-blue: 青っぽいテーマ

== 0.5.5から0.5.6の変更点: 2008-04-21

  * RTtoolの削除
  * 少しRuby 1.9対応
  * ポーズ対応

== 0.5.4から0.5.5の変更点: 2008-03-01

  * バグの修正
    * テストの修正（おばたさんによる報告）
    * --marginオプションの修正
  * RubyGemsでインストールされているHikiDocのサポート
  * Tgif関係のファイルが残っていたものを削除（おばたさんに
    よる報告）
  * 必要のないwindows-adjustテーマの削除
  * 索引ページ生成時の進行状況メッセージの表示をやめた
  * --log-levelオプションの追加
  * MacPorts用のドキュメントを更新
  * rabbit-mode.el:
    * 不要な変数の削除

== 0.5.3から0.5.4の変更点: 2007-12-15

  * ドキュメントの更新
    * ruby.stのURLを修正（きたさん）
    * MacPortsのドキュメントを更新（木村さん）
  * テーマ
    * night-blackテーマの色設定を追加
    * slide-logoテーマの追加
    * slide-footerテーマの追加
  * バグの修正
    * cairoバックエンドが利用可能かどうかのチェックを修正
    * Wikiパーサの読み込みエラーを修正（おばたさんによる報告）
    * Windows上での警告を除去
    * Windows上での"/"の扱いの修正（中井さん、おばたさん）
  * バックグラウンドでの作業状況をログに出力（おばたさんによる提案）
  * PDFをデフォルトの印刷フォーマットに変更
  * 箇条書きの印として「チェック」をサポート
  * --show-native-window-idオプションの追加
  * Wikiパーサでの<<< LANG\n...\n>>>のサポート
  * サンプルに引用を追加
  * 入力として画像ファイルをサポート
    （Rabbitが画像ビューアにもなる）
  * 画像の大きさの指定としてas_large_as_possibleを追加
  * setup.rb cleanでconfig.rbを削除（小林さん）
  * Tgifサポートの削除
  * --manオプションの追加（小林さん）
  * 入力としてslideshare.netのサポート
  * ドラッグアンドドロップのサポート

== 0.5.2から0.5.3の変更点: 2007-08-04

  * テーマ
    * Day White/Night Blackテーマの追加（たださん）
    * テーマ確認用スライドの追加（たださんによる提案）
    * 緑の円テーマの追加。
    * Auto SlideをSlide Showに変更。
    * テーマ（のコード）を綺麗に。
  * ドキュメントの更新
    * INSTALL.win32（むとうさん、zundaさん）
  * バグの修正
    * ページ一覧でのエスケープ漏れの修正（前田さんによる報告）
    * GRClosure絡みの問題の修正（やまださん）
    * ソース再読み込み時のクラッシュバグの修正
    * --output-index-htmlなしで--output-htmlを指定したときに発
      生するバグの修正。
    * 印刷時にフォントが反映されないバグの修正。
  * GIMPサポートの改良（やまださん）
  * libgnomeprintサポートの削除。
  * Wiki記法（HikiDoc）のサポート。
    （本リリースのみhikidoc.rbを同梱）
  * PS/PDFの品質が微妙である記述の削除。
  * 虫眼鏡のon/offをメニューに追加。
  * スポットライトのon/offをメニューに追加。
  * 部分レンダリングのサポート。
    （ソースレベルの文法は未サポート）
  * {start,stop}_reload_timer -> {start,stop}_redraw_timer
    （後方互換性のため{start,stop}_reload_timerも使用可能）

== 0.5.1から0.5.2の変更点: 2007-06-02

  * ドキュメントの更新
    * rabbit-mode.el（武田くん）
    * MacPorts（木村さん）
  * rabbit-mode.elの改良（武田くん）
    * rabbit-default-image-size-unit変数の追加
  * 起動に失敗するバグの修正（atzmさんによる報告）
  * その他、細かな修正（おばたさんによる報告）

== 0.5.0から0.5.1の変更点: 2007-03-29

  * 入力フォーマットとしてPDFサポートを追加。
  * rabbit-mode.elの改良。（武田くん）
    * rabbit-copy-slide: 追加
    * rabbit-duplicate-slide: 追加
  * フランス語のメッセージの更新。（スクリチくん）
  * 印刷時のフォント名の不具合の修正。
  * libgnomeprintサポートの廃止。

== 0.4.2から0.5.0の変更点: 2006-11-03

  * GtkGLExtのロード絡みのバグを修正．（かくたにさん）
  * rabbit-mode.elの改良．（武田くん）
  * emerge関係のドキュメントの更新．（あかぴさん）
  * MacPorts用のドキュメントの追加．（きむらさん）
  * RSVGのパス解決に対応策を追加．
  * （役に立たない）Ruby/Anthyのサポート．
  * マルチディスプレイ用情報ウィンドウのサポート．
  * フォント名にRabbit/Rabbit Monospaceがあったら，優先的に
    使うようにした．
  * アフィン変換系のAPIの追加．
    * canvas.rotate_context
    * canvas.scale_context
    * canvas.translate_context
    * canvas.reflect_context
    * canvas.shear_context
  * 描画情報を保存しておくAPIの追加．
    * canvas.save_context
    * canvas.restore_context
  * lightning-talkテーマでは"\n"で改行できるようにした．
  * aroundフックの追加．
  * 新しいテーマの追加．
    * rotate-zoom-effect
    * emphasize-keyword
    * scroll-effect
    * mirror-effect
  * スポットライト機能の追加．
    ((<URL:http://pub.cozmixng.org/~gallery/kou/screenshot/rabbit/spotlight/>))
  * 虫眼鏡機能の追加．
    ((<URL:http://pub.cozmixng.org/~gallery/kou/screenshot/rabbit/magnifier/>))
  * グラデーションのサポート．
  * setup.rb 3.4.1を使うように変更．

== 0.4.1から0.4.2の変更点

  * 検索時の正規表現のバグを修正．（西川さん）
  * ドキュメントを修正．（西川さん，きたさん）
  * rabbit-mode.elの追加（武田くん）
  * HTML生成のバグの修正．（かくたにさん）
  * アリスの画像を追加．（朝日奈さん）
  * rabbit-mode.lの修正．（みやむこさん，id:wata_dさん）
  * 高橋エイリアスから高橋メソッドへ．
  * Ruby/Popplerを用いたPDF表示のサポート．
  * OpenGLサポートの改良．
  * RabrickでのAJAXをサポート．
  * 携帯電話をサポート．
  * メモり使用を改善．

=== 他のニュース

  * きむらさんがMac OS Xのパッケージ（DarwinPorts用）を作ってくれました．

