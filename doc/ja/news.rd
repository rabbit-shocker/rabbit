---
layout: ja
title: お知らせ
apply_data: false
---
{% raw %}
== 3.0.0: 2019-09-06

GTK+ 3をサポートしたリリース。GTK+ 2のサポートは終了。

ワイド（16:9比率）スライドをサポートしたリリース。これは後方互換性のな
い変更なので注意。

=== 改良

==== rabbit

  * ワイド（16:9比率）スライドをサポート。

    ワイドスライドを使うには((%--size 800,450%))というように
    ((%--size%))オプションを指定します。

    これは後方互換性のない変更です。もし、すでにワイドサイズを指定して
    いた場合、スライドのテキストサイズが変わります。

    このリリースからは、通常（4:3比率）スライドとワイド（16:9比率）ス
    ライドで同じテキストサイズになります。このリリースより前のリリース
    ではテキストサイズが違っていました。

  * デフォルトでGTK+ 3を使うようになりました。

  * GTK+ 2のサポートを終了しました。

  * テキストをできるだけ大きくするときにテキストの折り返しなし指定を考
    慮するようにしました。

    これはスライドのレンダリング結果が変わるかもしれない変更です。

  * リロード時にタイマーをリセットしないようにしました。

  * (({start-time}))タイトルスライド属性と(({end-time}))タイトルスライ
    ド属性をサポートしました。

    (({start-time}))と(({end-time}))を両方指定している場合は
    (({allotted-time}))を指定する必要はありません。(({allotted-time}))
    はそれらの属性から計算できるからです。

    以下は1時間のトークを指定する例です。

      = タイトル

      : start-time
         2017-10-08T11:00:00+09:00
      : end-time
         2017-10-08T12:00:00+09:00

  * Hikiからスライドのソースを取得する機能を削除しました。

  * SlideShareからスライドのソースを取得する機能を削除しました。

  * RD：URLリンクのデフォルトテキストから(({<URL:...>}))というマークを
    削除しました。

    これはスライドのレンダリング結果が変わるかもしれない変更です。

  * アニメーションGIFをサポートしました。

  * 常に全面に表示することをやめました。
    ((%--no-keep-above%))オプションも削除しました。

    [Shugo Maedaさんが報告]

  * 情報ウィンドウ：テーマのリロードをサポートしました。
    [GitHub#118][Dominique Martinetさんが報告]

  * kramdown 2.1.0以降に対応しました。
    [GitHub#120][Yusuke Nakamuraさんが報告]

  * 矢印キーにmod2マスクがついてくる環境向けの回避策を入れました。
    [GitHub#121][Yukihiro Matsumotoさんが報告]

  * テーブルのセルでアライン用のタグを使えるようにしました。

  * (({Proc.new}))の警告がでないようにしました。
    [GitHub#124][Nobuyoshi Nakadaさんがパッチ提供]

  * ノートの中でリンクを使えるようにしました。

  * スライドの表示サイズを変えたときのテキストレンダリング方法を改良しました。
    サイズ変更による折り返し有無が変わることがなくなりました。

    [Yukihiro Matsumotoさんが報告]

==== rabbit-slide

  * ((%--licenses%))オプションを追加しました。
    [GitHub#116][Yusuke Nakamuraさんがパッチ提供]

  * GUIをサポートしました。

==== テーマ

  * clear-blue：デフォルトの落書き線の幅を細くしました。

  * lightning-talk-toolkit：
    (({@lightning_talk_wrap_mode}))をカスタマイズできるようにしました。

  * image-viewer：画像タイマーをサポートしました。

  * テーマの中でフォントサイズを計算するための(({font_size}))メソッド
    を追加しました。

==== ドキュメント

  * HTTPSを使うようにしました。
    [GitHub#122][Kazuhiro NISHIYAMAさんがパッチ提供]

=== 修正

==== rabbit

  * スライドのタイトルにファイル名として特別な文字があるときに印刷でき
    ない問題を修正しました。
    [GitHub#117][Dominique Martinetさんが報告]

==== ドキュメント

  * typoを修正しました。
    [GitHub#113][Hiroshi Hatakeさんがパッチ提供]

  * 壊れたリンクを修正しました。
    [GitHub#114][Yusuke Nakamuraさんがパッチ提供]

=== 感謝

  * Hiroshi Hatakeさん

  * Shugo Maedaさん

  * Yusuke Nakamuraさん

  * Dominique Martinetさん

  * Yukihiro Matsumotoさん

  * Kazuhiro NISHIYAMAさん

  * Nobuyoshi Nakadaさん

== 2.2.1: 2017-09-15

RubyKaigi 2017スピーカー向けリリース。

=== 改良

==== rabbit

  * Rougeに対応。

  * Ruby 2.4での警告をなくした。
    [GitHub#109][Nobuyoshi Nakadaさんがパッチ提供]

==== テーマ

  * image-slide-number: おまけスライドに対応。
    「image-slide-number-last-slide」スライドプロパティーに「true」を指定
    するとそのスライドを最後のスライドとして画像タイマーの位置を計算する。
    このスライド以降は画像タイマーは進まない。

==== ドキュメント

  * RD用のサンプルを追加。
    [GitHub#105][Masayuki Morisakiさんがパッチ提供]

  * 開発環境の用意の仕方を追加。
    [GitHub#106][Masayuki Morisakiさんがパッチ提供]

  * すでに使っていないprototype.jsのライセンス情報を削除。
    [GitHub#107][Ryunosuke Satoさんがパッチ提供]

=== 修正

==== rabbit

  * 情報ウィンドウ：ソースのフォーマット検出に失敗する問題を修正。
    [GitHub#102][Akihisa Higuchiさんがパッチ提供]

==== ドキュメント

  * サンプルページのスタイルを修正。
    [GitHub#103][takiy33さんが報告]

=== 感謝

  * Akihisa Higuchiさん

  * takiy33さん

  * Masayuki Morisakiさん

  * Ryunosuke Satoさん

  * Nobuyoshi Nakadaさん

== 2.2.0: 2017-01-22

Markdownサポートを強化したリリース。

=== 改良

==== 全体

  * Ruby 2.0のサポートをやめた。

  * gdk_pixbuf2 gem 3.0.8以前のサポートをやめた。

==== rabbit

  * PDF：「%」が含まれているファイル名に対応。

  * Migemoサポートを削除した。

  * Markdown：タグをサポート。

    例（インライン）：

      普通のテキスト{::tag name="center"}中央寄せされたテキスト{:/tag}普通のテキスト

    例（段落単位）：

      普通のテキスト

      {:.center}
      中央寄せされたテキスト

      普通のテキスト

  * Markdown：ネストしたリストの項目での(({wait}))をサポート。

  * 動的にフォントサイズを決めるときのパフォーマンスを改善。

  * スライド・テーマのgemをpushするときに(({config.yaml}))で指定してい
    るRubyGemsユーザーを使うようにした。

  * PDF：リンクをサポート。ただし、以下の制限がある。
      * cairo 1.15.4以降が必要。（1.15.4未満の場合は単にこの機能を使えないだけ。）
      * リンクテキストだけでなく段落全体がリンクになってしまう。

==== テーマ

  * slide-logo：(({logo-image}))スライドプロパティーに画像ファイル名を
    指定することでスライド毎にロゴを変えられるようにした。

  * title-on-image-toolkit：文字の縁も描画して読みやすくした。

  * syntax-highlighting：Cの(({#include}))のパスの色を通常の文字列と同
    じ色にした。

==== rabbit-slide

  * Markdown：必要な改行が出力されていない問題を修正。
    [GitHub#95][znzさんがパッチ提供]

==== ドキュメント

  * FAQの英語を改良。
    [GitHub#100][Todd Trimbleさんがパッチ提供]

=== 修正

==== rabbit

  * マルチバイトの画像ファイル名を指定してそのファイルが見つからなかっ
    たときに適切にエラーを報告できない問題を修正。
    [GitHub#91][takiy33さんが報告]

  * Windows：画像ファイル検出時のエンコーディングエラーを修正。
    [GitHub#93][dogatanaさんが報告]

  * スライド選択ダイアログでキャンセルしたときにエラーが発生していた問
    題を修正。

  * typoを修正。
    [GitHub#97][znzさんがパッチ提供]

  * スライドのアスペクト比とウィンドウのアスペクト比が異なるときにスポッ
    トライト・虫眼鏡が対象とする位置がずれる問題を修正。
    [GitHub#99][おばたさんがパッチ提供]

  * Windows：フルスクリーンを解除してもウィンドウサイズが戻らない問題
    を修正。
    [GitHub#98][ただただしさんが報告]

==== ドキュメント

  * typoを修正。
    [GitHub#101][znzさんがパッチ提供]

=== 感謝

  * takiy33さん
  * dogatanaさん
  * znzさん
  * Todd Trimbleさん
  * おばたさん
  * ただただしさん

== 2.1.9: 2016-05-27

Markdownサポートを強化したリリース。

=== 改良

==== rabbit

  * Ruby 2.3での警告を削除。
    [GitHub#77][takiy33さんがパッチ提供]

  * Markdownマークアップ: GFM（GitHub Flavored Markdown）での取り消し
    線マークアップに対応した。
    [yoku0825さんが提案]

    構文:

      ~~deleted text~~

  * Markdownマークアップ: 直接HTMLを記述する方法をサポートしていないた
    め、HTMLを書いたらエラーになるようにした。

  * エスケープキーで検索を止められるようにした。

  * RDマークアップ: (({pango})) block verbatimをサポートした。
    ((<Pangoのマークアップ|URL:https://developer.gnome.org/pango/stable/PangoMarkupFormat.html>))をしたテキストを指定できる。

    構文:

      # pango

      <span foreground="red">Red Text</span>

  * ファイル選択ダイアログでMarkdownファイルのみを絞り込めるようにした。
    [GitHub#83][tSU_Rootさんがパッチ提供]

  * Markdownマークアップ: サポートしていない平行線マークアップを使った
    場合はエラーを報告するようにした。
    [GitHub#84][tSU_RooTさんが報告]

  * Markdownマークアップ: リスト内でのリストをサポートした。
    [GitHub#85][tSU_RooTさんが報告]

==== テーマ

  * default-title-text: (({@title_slide_font_size}))で作者のフォントサ
    イズをカスタマイズできるようにした。

  * background-image-toolkit: 縦方向の整列方法として(({bottom}))をサポー
    トした。

  * image-viewer: コメントをサポートした。

  * syntax-highlighting: diff形式をサポートした。

  * syntax-highlighting: 数字を色付けすることをやめた。

  * clear-blue: シンタックスハイライト部分の枠の色を指定した。

==== ドキュメント

  * ライセンスの詳細をリンクにした。
    [GitHub#86][tSU_Rootさんがパッチ提供]

  * Markdownでのスライドの書き方ドキュメントを更新した。
    [GitHub#89][tSU_Rootさんがパッチ提供]

=== 修正

==== rabbit

  * 不正な内容のスライドを指定するとクラッシュする問題を修正した。
    [GitHub#76][takiy33さんが報告]

  * (({--margin}))オプションの値のパース方法が間違っていた問題を修正し
    た。
    [GitHub#82][zundaさんがパッチ提供]

==== テーマ

   * image: Markdownマークアップとimage-timerテーマを一緒に使うと画像
     が小さくなる問題を修正した。
     [GitHub#78][とみたまさひろさんが報告]

   * image: キャプション付き画像のサイズが安定しない問題を修正した。
     [GitHub#88][とみたまさひろさんが報告]

=== 感謝

  * takiy33さん

  * yoku0825さん

  * とみたまさひろさん

  * zundaさん

  * tSU_Rootさん

== 2.1.8: 2015-09-06

2.1.7がWindowsで動かない問題を修正したリリース

=== 修正

==== rabbit

  * Windows: 起動できない問題を修正。 [yoku0825さんが報告]

=== 感謝

  * yoku0825さん

== 2.1.7: 2015-09-05

Markdownでのノートマークアップ対応リリース。

=== 改良

==== rabbit

  * GDKレンダリングエンジンを削除。（だいぶ前からcairoが必須になってい
    てGDKレンダリングエンジンがなくても問題なくなっていたため。）
  * GTK+ 3対応を進めた。（まだ完成していない。） [okkezさんがパッチ提供]
  * 使われていない(({--server}))オプションを削除。
  * Wikiマークアップ: 未サポートの書き方のときのエラーメッセージを改善。
  * Markdownマークアップ: 1つの段落内に複数の画像を指定したときに「未
    サポートである」とメッセージを出力するようにした。
    [GitHub#71][Colin Deanさんが報告]
  * Markdownマークアップ: ノートマークアップ対応。 [yoku0825さんが提案]

    書式は(('{::note}...{:/note}'))です。

    例：

      普通のテキスト{::note}ノート{:/note}普通のテキスト

==== テーマ

  * image-timer: メモリー使用量を削減。
  * image-slide-number: メモリー使用量を削減。
  * clear-blue: 引用ブロックでの最初の行のインデントをやめた。

==== ドキュメント

  * READMEをMarkdownでマークアップした。
    [GitHub#72][Toshi MARUYAMAさんがパッチ提供]
  * Rabbitの概要の説明を改良。 [GitHub#74][YU-TAYUさんがパッチ提供]

=== 修正

==== rabbit-slide

  * 不正なMarkdownを出力する問題を修正。 [Matafumi Yokoyamaさん]

==== ドキュメント

  * サイドメニューが隠れている問題を修正。
    [GitHub#75][YU-TAYUさんが報告]

=== 感謝

  * okkezさん
  * Matafumi Yokoyamaさん
  * Colin Deanさん
  * Toshi MARUYAMAさん
  * YU-TAYUさん
  * yoku0825さん

== 2.1.6: 2015-02-26

2.1.5のバグフィックスリリース。

=== 修正

==== rabbit

  * フルスクリーン機能が動作しない問題を修正。
    [GitHub#58] [Mamoru TASAKAさんが報告]

=== 感謝

  * Mamoru TASAKAさん

== 2.1.5: 2015-02-25

2.1.4のバグフィックスリリース。

=== 改良

==== rabbit

  * 古いGTK+ 2のためのコードを削除した。
    [GitHub#56] [okkezさんがパッチ提供]
  * GTK+ 3サポートのためのコード整理を開始。
    [GitHub#55] [okkezさんがパッチ提供]

==== テスト

  * blockdiagがインストールされていなくてもエラーにならないようにした。
    [GitHub#54] [Mamoru TASAKAさんが報告]

=== 修正

==== rabbit

  * 情報ウィンドウに適切なサイズで表示されない問題を修正。

=== 感謝

  * Mamoru TASAKAさん
  * okkezさん

== 2.1.4: 2015-02-23

Markdownサポートを改善したリリース。

=== 改良

==== rabbit

  * ログメッセージ中の不正なエンコーディングの文字を置き換えるようにした。
    [shocker-ja:1228] [おばたさんが報告]
  * cairoスクリプトの出力に対応した。出力ファイルの拡張子を.csにすると
    cairoスクリプト形式で出力される。
    * 例:
        % rabbit --print --output-filename=slide.cs
  * 不要なスペースを削除。
    [GitHub#48] [Yuichi NANSAIさんがパッチ提供]
  * インデントをスペースで統一。
    [GitHub#49] [Yuichi NANSAIさんがパッチ提供]
  * PDFでスライドの縦横比を維持するようにした。
    [Kenshi Mutoさんが提案]
  * (({file:///})) URIに対応。
    [TOMITA Masahiroさんが報告]
  * WindowsでもPDFを読み込めるようにした。
    [TOMITA Masahiroさんが報告]
  * Markdown: waitタグをサポート。
    [shocker-ja:1249] [Isobeさんが提案]
    * 例:
        {::wait/}
  * Markdown: 順序付きリストをサポート。
  * Markdown: コードブロックフェンスでのシンタックスハイライトに対応。
    * Kramdownスタイルの例:
        ~~~ruby
        # Rubyコード
        ~~~
    * GitHub Flavored Markdownスタイルの例:
        ```ruby
        # Rubyコード
        ```
  * Markdown: シンタックスハイライトの言語指定の属性名として
    (({language}))もサポート。
    * 例:
        # タイトル

            def hello
            end
        {: language="ruby"}
  * Markdown: blockdiagに対応。
    * 例:
        ```blockdiag
        {
          A -> B -> C;
        }
        ```
  * 後方互換性のための不要なコードを削除。
  * ウィンドウサイズを変えてもスライドの初期表示サイズの縦横比を維持す
    るようにした。

==== rabbit-slide

  * ISO 8601形式の日付フォーマットを使うようにした。

==== テーマ

  * default-block-quote: ASCIIのみの引用テキストは両端揃えにしないよう
    にした。

==== ドキュメント

  * OS Xでフルスクリーン機能を使う方法を追加。
    [GitHub#45] [Shinta Koyanagiさんがパッチ提供]
  * Ruby Installerのバージョンを更新。 [Masafumi Yokoyamaさん]
  * Rabbitを使ったスライドのURLを追加。
    [GitHub#46] [Brett Chalupaさんがパッチ提供]
  * typoを修正。
    [GitHub#47] [HAYASHI Kentaroさんがパッチ提供]
  * Windows向けの古い情報を削除。
    [SATOH Kiyoshiさんが報告]

=== 修正

==== rabbit

  * PDFを読み込めない問題を修正。
    [Junichi Oyaさんが報告]
  * Markdown: メタ文字のエスケープ漏れを修正。
    [GitHub#50] [Matthias Güntherさんが報告]

=== 感謝

  * おばたさん
  * Junichi Oyaさん
  * Shinta Koyanagiさん
  * Brett Chalupaさん
  * Yuichi NANSAIさん
  * Kenshi Mutoさん
  * Matthias Güntherさん
  * HAYASHI Kentaroさん
  * TOMITA Masahiroさん
  * SATOH Kiyoshiさん

== 2.1.3: 2014-08-03

描画速度を改善したリリース。

=== 改良

==== rabbit

  * UTF-8な入力ファイルのエンコーディング検出処理を改良。
    [GitHub#34] [Colin Deanさんが報告]
  * Markdown: 項目の内容がないリストをサポート。
    [GitHub#37] [Colin Deanさんが報告]
  * 縦方向の中央揃えを使っていない時の描画性能を改善。
    [GitHub#35] [Colin Deanさんが報告]
  * メモリー使用量の増加を抑えた。o
    [GitHub#41] [Enrico Rivarolaさんが報告]

==== テーマ

  * default-preformatted: (({PreformattedText}))ではなく、
    (({PreformattedBlock}))にテキストサイズを設定するように変更。
  * tag: (({xx-small}))タグと(({xx-large}))タグに対応。
    [GitHub#39] [Enrico Rivarolaさんがパッチ提供]
  * syntax-highlighting: 浮動小数点リテラルに対応。

==== ドキュメント

  * Ruby Installerのバージョンを更新。 [Masafumi Yokoyamaさん]
  * Homebrewでのインストール方法を更新。
    [GitHub#30] [Bert Changさんがパッチ提供]
  * 英語の翻訳と改良。
    [GitHub#33] [Tomohiro Imaizumiさんがパッチ提供]
  * Windowsでは64bit版Rubyに未対応であるという説明を追加。
    [GitHub#43] [YUKI Hiroshiさんがパッチ提供]

=== 修正

==== rabbit

  * rabbitコマンドの終了ステータスが常に0以外の問題を修正。
  * ウィンドウシステムがない環境で起動するとエラーを出力して終了するのではなく、
    クラッシュする問題を修正。
    [shocker-ja:1189] [Kazuhiro NISHIYAMAさんが報告]
  * 情報ウィンドウ: ウィンドウサイズが変更してもノートテキストの大きさ
    が反映されない問題を修正。
    [shocker-en:71][shocker-en:74] [Enrico Rivarolaさんが報告]
  * 情報ウィンドウ: ノートに長い単語があると切れてしまう問題を修正。
    [shocker-en:78] [Enrico Rivarolaさんが報告]
  * 情報ウィンドウ: ノートのマークアップが反映されない問題を修正。
    [GitHub#38] [Enrico Rivarolaさんがパッチ提供]
  * 一時ファイルをすぐに削除しない問題を修正。
    [GitHub#40] [Enrico Rivarolaさんがパッチ提供]

==== rabbit-slide

  * SlideShareに公開するときにタグが反映されない問題を修正。

=== 感謝

  * Masafumi Yokoyamaさん
  * Kazuhiro NISHIYAMAさん
  * Bert Changさん
  * Enrico Rivarolaさん
  * Colin Deanさん
  * YUKI Hiroshiさん

== 2.1.2: 2014-03-08

2.1.1のバグフィックスリリース。

=== 改良

==== rabbit

  * 情報ウィンドウがソースの自動再読み込みに対応。
    [GitHub#23] [Kazuhiro NISHIYAMAさんが報告]
  * ソースの構文をチェックして終了する((%--check-syntax%))オプションを追加。
    [GitHub#27] [HAYASHI Kentaroさんが提案]
  * Markdownでインラインコード用の「(({`...`}))」記法を使えるようになった。
    [GitHub#29] [KITAITI Makotoさんがパッチを提供]
  * Markdownで改行用の「(({\\}))」記法を使えるようになった。
    [GitHub#29] [KITAITI Makotoさんがパッチを提供]

==== rabbit-slide

   * Markdownで書かれたREADMEに対応。
   * Gitのタグを打つ「tag」タスクを追加。
   * 「theme.rb」ファイルがあったら自動でgemに追加するようにした。
   * config.yamlにプレゼンテーションに対応するYouTubeの動画を指定する
     (({youtube_id}))パラメーターを書けるようにした。指定すると
     slide.rabbit-shocker.orgでリンクが出る。

==== ドキュメント

  * Homebrewでのインストール方法を更新。
    [Yutaro Sugaiさん]
  * Windowsでのインストール方法を更新。
    [Masafumi Yokoyamaさん]

=== 修正

==== rabbit

  * 落書きモードの設定ダイアログを開けない問題を修正。
    [shocker-en:63] [Enrico Rivarolaさんが報告]

==== rabbit-slide

  * PDFを作らなくてもgemを作れてしまう問題を修正。

=== 感謝

  * Kazuhiro NISHIYAMAさん
  * Yutaro Sugaiさん
  * Masafumi Yokoyamaさん
  * HAYASHI Kentaroさん
  * Enrico Rivarolaさん
  * KITAITI Makotoさん

== 2.1.1: 2013-06-26

2.1.0のバグフィックスリリース。

=== 修正

==== rabbit

  * 起動時にNameErrorがでて起動できない問題を修正。
    [GitHub#25] [Yoshihide Chubachiさんが報告]

=== 感謝

  * Yoshihide Chubachiさん

== 2.1.0: 2013-06-16

2.0.9のバグフィックスリリース。

=== 改良

==== rabbit

  * READMEファイルを検出するときにバックアップファイルを無視するようにした。
    [GitHub:#21] [TOMITA Masahiroさんが報告]
  * Ruby 1.8でインストールしようとしたらRubyGemsがエラーを報告するようにした。

=== 修正

==== rabbit

  * エンコーディング変換時のエラー処理が動かなかった問題を修正。
    [Junichi Oyaさんが報告]
  * Ruby/GLib2 2.0.2でも動作するようにした。

=== 感謝

  * TOMITA Masahiroさん
  * Junichi Oyaさん

== 2.0.9: 2013-06-16

起動まわりを修正したリリース。

=== 改良

==== 全体

  * Ruby 1.8のサポートを終了した。

==== rabbit

  * GLibのエンコーディング変換機能ではなくRubyのエンコーディング変換機
    能を使うようにした。

==== テーマ

  * tag: スタイルを変更するタグをサポート。

      (('tag:normal:XXX'))
      (('tag:oblique:XXX'))
      (('tag:italic:XXX'))

=== 修正

==== rabbit

  * 環境によって起動できない問題を修正。
    [shocker-ja:1128] [znzさんが報告]
    [GitHub:#19] [Steve Klabnikさんが報告]
  * UTF-8以外のロケールでコマンドラインオプションのヘルプメッセージを表
    示できない問題を修正。
    [shocker-ja:1109] [OBATA Akioさんが報告]
    [Masafumi Yokoyamaさんがパッチ作成]

=== 感謝

  * znzさん
  * Steve Klabnikさん
  * Masafumi Yokoyamaさん
  * OBATA Akioさん

== 2.0.8: 2013-06-01

テーマ関連を細々と改良したリリース！

=== 改良

==== rabbit

  * .rbtもRDのソースファイルとして受け付けるようにした。
    [socker-ja:1109] [OBATA Akioさんが報告]
  * 画像ファイルのパスに絶対パスを指定できるようにした。
  * Rabbit::Element::Base#have_tag?を追加。

    これは要素がカスタムタグを持っているかを探したいときに便利です。
    例:

        if element.have_tag?("as-large-as-possible")
          elsement.as_large_as_possible
        end

==== テーマ

  * ディレクトリーテーマスタイルのときにカレントディレクトリのファイルを
    データファイルとして使えるようにした。
    ディレクトリーテーマスタイルとは名前が「.」のテーマです。例:
       : theme
          .

  * $LOAD_PATHにあるテーマでもTHEME_DIR/data/をデータディレクトリーとして
    使えるようにした。これはgemで提供しているテーマと同じルールです。

  * syntax-highlighting: サポートしているシンタックスを追加。

  * tag: 「left」タグをサポート。

  * tag: 「margin-top * N」をサポート。 margin-leftとmargin-bottomと
    margin-rightも同様。

=== 感謝

  * OBATA Akioさん
  * Masafumi Yokoyamaさん

== 2.0.7: 2013-04-29

安定性向上リリース！

=== 改良

==== パッケージ

  * [GitHub#13] 不要なファイルをパッケージに含めないようにした。
    [Youhei SASAKIさんが報告]
  * Ruby/GStreamerを依存パッケージから外した。
    [rabbit-shocker:1089] [znzさんが報告]

==== rabbit

  * 不必要な実行属性を削除した。
  * Mac OS X 10.6.8でクラッシュするため、必要なときだけRuby/GStreamerを
    読みこむようにした。
    [masaさんが報告]
  * 拡張子でソースのマークアップが判断できる場合は、誤検出を避けるため
    ソースの内容から推測しないようにした。
  * フルスクリーン・フルスクリーン解除のフォールバック機能を削除した。
    最近のGTK+ではフォールバックが必要なことがないため。
    [Youhei SASAKIさんが報告]
  * 非ASCIIのファイル名をサポート。
    [GitHub#15][GitHub#16][GitHub#17] [Masafumi Yokoyamaさんがパッチ提供]
  * iconvの代わりにString#encodeを使うようにした。
    [GitHub#18] [Masafumi Yokoyamaさんがパッチ提供]

==== テーマ

  * lightning-talk-toolkit: as-large-as-possibleスライドプロパティをサ
    ポート。

      そのスライドだけ有効にする場合:

        = 大きなタイトル

        == プロパティ

        : as_large_as_possible
           true

      そのスライドだけ無効にする場合:

        = 大きなタイトル

        == プロパティ

        : as_large_as_possible
           false

==== テスト

  * $LOAD_PATHが足りなかった問題を修正。
    [GitHub#14] [Masafumi Yokoyamaさんが報告]

==== rabbit-slide

  * 複数段落の説明文に対応。
  * SlideShareの情報を使うため、RubyGems.orgへの公開を最後にした。
    [Masafumi Yokoyamaさん]
  * RubyGems 2.0対応。 [Masafumi Yokoyamaさん]

=== 感謝

  * Youhei SASAKIさん
  * masaさん
  * Masafumi Yokoyamaさん
  * znzさん

== 2.0.6: 2012-12-29

ビデオサポートリリース！2012年最後のリリース！

=== 改良

==== rabbit-slide

  * Ustreamに対応。
  * Vimeoに対応。
  * SlideShareでのURLにはタイトルではなくIDを使うようにした。
    [Masafumi Yokoyamaさんがパッチ提供]
  * changeコマンドで設定を変更できるようにした。

==== ドキュメント

  * RDを使った時の画像の使い方を追加。 [ほっかいさんが追加]

==== rabbit

  * ファイルの拡張子もチェックしてPDFを認識するようにした。
  * [非互換] 画像としてPDFを使うときに指定するページ番号を0スタートでは
    なく1スタートにした。これは、PDFが1ページからカウントするので、それ
    にあわせるためである。
  * [実験的] スライドにビデオを埋め込めるようにした。
    [Narihiro Nakamuraさんがパッチ提供]
  * テキストの影を元のテキストから離れ過ぎないようにした。

==== rabbit-theme-manager

  * 必要がないので削除。

=== 修正

==== rabbit

  * .rabbitディレクトリを起動オプションを指定したファイルとはみなさない
    ようにした。 [Koichi Akabeさんが報告]

=== 感謝

  * Masafumi Yokoyamaさん
  * Koichi Akabeさん
  * ほっかいさん
  * Narihiro Nakamuraさん

== 2.0.5: 2012-09-14

バグフィックスリリース。

=== 修正

==== テーマ

  * リサイズ済みの画像が表示されない問題を修正
    [rabbit-shocker:1057] [znzさんが報告]

=== 感謝

  * znzさん

== 2.0.4: 2012-09-12

バグフィックスリリース。

=== 改良

==== rabbit-slide

  * --titleで指定した値を生成するスライドのタイトルにした。
    [znzさんが報告]

==== テーマ

  * 引用記号画像の解像度を大きくした。

=== 修正

==== rabbit-slide

  * SlideShareへアップロードするrake publish:slideshareが動かない問題を修正。
    [GitHub#8] [myokoymさんがパッチ提供]

==== テーマ

  * 画像の鏡面反射が効かない問題を修正。

=== 感謝

  * znzさん
  * myokoymさん

== 2.0.3: 2012-09-10

引用のみばえがよくなったリリース。

=== 改良

==== 全体

  * ユーザーグループIDを「rabbit-shocker」に変更した。
  * ユーザーグループ名を「Rabbitショッカー」に変更した。
  * rabbit.gemspecをgemに含めるようにした。

==== rabbit

  * gzipで圧縮されたDiaファイルをサポート。 [okkezさんが報告]
  * Diaファイルを表示するときはSVGに変換して表示するようにした。
  * PDF出力時のサイズをA4横から縦横比4:3の360mm,270mmに変更した。

==== rabbit-slide

  * SlideShareでのスライドIDを保存するようにした。
  * Speaker DeckでのスライドIDを保存するようにした。
  * 出力するPDFファイル名を"#{スライドのID}-#{スライドのベース名}.pdf"
    にした。

==== rabbit-theme

  * 英語と日本語のテーマベンチマーク用スライドのPDFを保存するようにした。

==== テーマ

  * インデントの値にRabbit::Format::Sizeも指定できるようになった。
  * 画像のキャプションの文字の色を変更できるようにした。
  * title-on-image-toolkit: タイトルに背景色をつけるようにした。
  * default-block-quote:
    * @block_quote_image_max_widthを非推奨とした。代わりに
      @block_quote_image_widthを使ってください。
    * 枠線の代わりに画像を使えるようにした。
    * 引用元を示す文字列のフォントサイズを小さくした。
    * 引用文を両端揃いにするようにした。
    * アバターを設定できるようにした。
  * clear-blue:
    * 引用ブロックの最初の段落をインデントするようにした。
    * 枠線の代わりに画像を使うようにした。
  * color-circle-block-quote:
    * 枠線の代わりに画像を使うようにした。
  * rabbit-block-quote:
    * 枠線の代わりに画像を使うようにした。

==== ドキュメント

  * Homebrewのインストール方法を更新した。 [菅井祐太朗さんが更新]

=== 修正

==== rabbit-slide

  * slide.rabbit-shocker.orgでのスライドURLを修正。

==== rabbit-theme

  * theme.rabbit-shocker.orgでのテーマURLを修正。

=== 感謝

  * 菅井祐太朗さん
  * okkezさん

== 2.0.2: 2012-09-02

スライド・テーマ管理機能を使いやすくしたリリース。

2.0.1以前とは非互換になっています。2.0.1以前で生成したスライド・テーマ
がある場合は変更点にあるRakefileとconfig.yamlを参考に手動でアップグレー
ドしてください。自動アップグレード機能はありません。

=== 改良

==== rabbit-slide

  * Rakefileを短くした。（非互換）

      require "rabbit/task/slide"
      Rabbit::Task::Slide.new

  * 設定はconfig.yamlで行うようにした。（非互換）

      ---
      id: theme-benchmark-en
      base_name: theme-benchmark
      tags:
      - rabbit
      presentation_date: 2012/09/02
      version: 1.0.0
      licenses:
      - GPLv3+
      - GFDL
      - CC BY-SA 3.0
      author:
        markup_language: :rd
        name: Kouhei Sutou
        email: kou@cozmixng.org
        rubygems_user: kou
        slideshare_user: kou
        speaker_deck_user: kou

  * ライセンスを指定できるようにした。
  * .gitignoreも生成するようにした。
  * gemにPDFも含めるようにした。

==== rabbit-theme

  * Rakefileを短くした。（非互換）

      require "rabbit/task/theme"
      Rabbit::Task::Theme.new

  * 設定はconfig.yamlで行うようにした。（非互換）

      ---
      id: clear-blue
      tags:
      - rabbit
      version: 1.0.0
      licenses:
      - GPLv3+
      - GFDL
      - CC BY-SA 3.0
      author:
        name: Kouhei Sutou
        email: kou@cozmixng.org
        rubygems_user: kou

  * ライセンスを指定できるようにした。
  * .gitignoreも生成するようにした。

==== テーマ

  * default-title-text: 「所属」の下にマージンを設定した。
  * default-title-text: 「出典」の下からマージンを削除した。
  * slide-number: デフォルトのマージンではなくスライドに設定したマージ
    ンを使うようにした。

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

==== rabbiter

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
    ない問題の修正 [rabbit-shocker:654] [おばたさん]
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

  * GtkGLExtのロード絡みのバグを修正。（かくたにさん）
  * rabbit-mode.elの改良。（武田くん）
  * emerge関係のドキュメントの更新。（あかぴさん）
  * MacPorts用のドキュメントの追加。（きむらさん）
  * RSVGのパス解決に対応策を追加。
  * （役に立たない）Ruby/Anthyのサポート。
  * マルチディスプレイ用情報ウィンドウのサポート。
  * フォント名にRabbit/Rabbit Monospaceがあったら、優先的に
    使うようにした。
  * アフィン変換系のAPIの追加。
    * canvas.rotate_context
    * canvas.scale_context
    * canvas.translate_context
    * canvas.reflect_context
    * canvas.shear_context
  * 描画情報を保存しておくAPIの追加。
    * canvas.save_context
    * canvas.restore_context
  * lightning-talkテーマでは"\n"で改行できるようにした。
  * aroundフックの追加。
  * 新しいテーマの追加。
    * rotate-zoom-effect
    * emphasize-keyword
    * scroll-effect
    * mirror-effect
  * スポットライト機能の追加。
    ((<URL:http://pub.cozmixng.org/~gallery/kou/screenshot/rabbit/spotlight/>))
  * 虫眼鏡機能の追加。
    ((<URL:http://pub.cozmixng.org/~gallery/kou/screenshot/rabbit/magnifier/>))
  * グラデーションのサポート。
  * setup.rb 3.4.1を使うように変更。

== 0.4.1から0.4.2の変更点

  * 検索時の正規表現のバグを修正。（西川さん）
  * ドキュメントを修正。（西川さん，きたさん）
  * rabbit-mode.elの追加（武田くん）
  * HTML生成のバグの修正。（かくたにさん）
  * アリスの画像を追加。（朝日奈さん）
  * rabbit-mode.lの修正。（みやむこさん，id:wata_dさん）
  * 高橋エイリアスから高橋メソッドへ。
  * Ruby/Popplerを用いたPDF表示のサポート。
  * OpenGLサポートの改良。
  * RabrickでのAJAXをサポート。
  * 携帯電話をサポート。
  * メモり使用を改善。

=== 他のニュース

  * きむらさんがMac OS Xのパッケージ（DarwinPorts用）を作ってくれました。
{% endraw %}
