---
layout: ja
title: rabbitコマンドの使い方
---
== 起動モード

Rabbitはコマンドラインから起動する方法とコマンドランチャーか
ら起動する方法（例えば、ダブルクリックで起動する方法）があり
ます。どちらも((%rabbit%))コマンドを使うことになります。

== コマンドライン

コマンドラインから起動する場合は引数にスライドを書いたファイ
ルを指定してください。

  % rabbit XXX.rd

例えば，sample/rabbit-implementation.rdを使うには，トップの
ディレクトリで以下のようにします．

  % rabbit sample/rabbit-implementation.rd

ただし，いくつかのサンプルは，テーマがsample/rabbit/theme以
下にあるためトップからではうまく表示できないかもしれません．
サンプルの実行は sample ディレクトリに入ってするとよいでしょ
う．

WindowsユーザならRDファイルをbin/rabbit.batにドラッグアンド
ドロップしてもよいです．

あるいは，*.rd（とか*.rab（Rabbitの略））を

  <rubyをインストールしたフォルダ>\bin\rubyw -S rabbit

に関連付けて，RDファイルをダブルクリックすることで起動するこ
ともできます．

=== オプション

: -t, --theme=THEME
   テーマを指定します．

: -I, --include=PATH
   ロードパスを追加します．ロードパスはテーマの検索などにも
   使用されます．

: --comment-theme=THEME
   コメント用のテーマを指定します。現在利用可能なテーマはス
   ライド下部にコメントを表示するfooter-comment（デフォルト）
   と、スライド上にコメントを流すstream-commentです。

: --allotted-time=TIME
   プレゼン時間を指定します。タイトルスライドにメタデータと
   して指定したプレゼン時間よりも優先されます。

: -B, --base=BASE
   入力ソース中の相対パス（例えば画像へのパス）を解決するた
   めのURIまたはパスを指定します．省略した場合は入力ソースの
   種類に応じて自動的に決定されます．

: -T, --type=TYPE
   入力ソースの種類を指定します．

   入力ソースの種類は file（デフォルト）, argf, uri 
   のいずれかを指定します。

   : argf

     入力ソースとしてARGFを利用します．つまり，標準入力から
     ソースを入力することも出来ます．
     
     ソースの自動再読み込み機能は利用できません．
     
     例:
       % rabbit --type argf file1.rd file2.rd ...
     とか
       % cat file1.rd file2.rd ... | rabbit --type argf
     とか
       % rabbit --type argf
       = title
       ...
       ^D
     とか

   : file

     デフォルト．つまり--type fileは省略可．
     
     指定したファイルからソースを取得します．

     ソースの自動再読み込み機能を利用できます．
     
     例:
       % rabbit --type file file.rd

   : uri

     指定したURIからソースを取得します．

     ソースの自動再読み込み機能を利用できますが，パフォーマ
     ンスなどを考慮して，最後にソースを読み込んでから一定時
     間（現在は60秒）以上たたないとたとえソースが変更されて
     いてもソースを再読み込みしません．

     例:
       % rabbit --type uri https://raw.github.com/shockers/rabbit/master/sample/rabbit-implementation.rd
     とか
       % rabbit --type uri ftp://.../XXX.rd
     とか

     ちなみに，
       % rabbit --type uri file:///.../XXX.rd
     とか
       % rabbit --type uri /.../XXX.rd
     とかは
       % rabbit --type file /.../XXX.rd
     と同じ

   : memory

     ソースをメモリ上に置いて管理します．このタイプではdRuby
     やSOAPなどのインターフェイスを用いてソースの内容を書き
     換えることができます．
     
     初期ソースはファイル名を指定して，そのファイルの内容を
     与えることができます．

     例（初期ソースなし）:
       % rabbit --type memory

     例（初期ソースあり）:
       % rabbit --type memory file.rd

: -e, --encoding=ENCODING
   入力ソースのエンコーディングを指定します．
   
   指定しなかった場合は自動検出を試みます．

: -f, --full-screen, --no-f, --no-full-screen
   フルスクリーンモードで起動するかどうかを指定します．
   
   デフォルトではフルスクリーンモードにはなりません．

: --index-mode, --no-index-mode
   一覧モードで起動するかどうかを指定します．
   
   デフォルトでは一覧モードにはなりません．

: -w, --width=WIDTH
   ウィンドウの幅を指定します．
   
   デフォルトは800です．
   
: -h, --height=HEIGHT
   ウィンドウの高さを指定します．

   デフォルトは600です．
   
: -S, --size=WIDTH,HEIGHT
   ウィンドウの高さと幅を指定します．

: -s, --save-as-image
   各スライドを画像として保存し，終了します．

: -i, --saved-image-type=TYPE
   保存される画像の種類を指定します．
   
   例えば，png（デフォルト）とかjpegとか．

: -b, --saved-image-base-name=BASE_NAME
   保存される画像のファイルのベース名を指定します．保存され
   る画像の名前は"#{ベース名}#{ページ番号}.#{拡張子}"となり
   ます．
   
   デフォルトはスライドのタイトルです．
   
   もし，ファイルシステムのエンコーディングがUTF-8ではないの
   に，ファイル名がUTF-8で保存される場合は，環境変数LANGとか
   G_FILENAME_ENCODINGとかを設定するとよいかもしれません．

: --output-html, --no-output-html
   保存されたスライドを表示するHTMLを生成するかどうかを指定
   します．
   
   デフォルトでは生成しません．

: --output-index-html, --no-output-index-html
   保存されたスライドのサムネイルを表示するHTMLを生成するか
   どうかを指定します．
   
   デフォルトでは生成しません．

: -p, --print
   スライドを印刷し，終了します．--output-filenameを指定する
   ことによりファイルに印刷したり，プリンタに直接印刷するこ
   ともできます．
   
   ただし，現在のところ，あまり品質はよくありません．

: -o, --output-filename=FILENAME
   印刷ファイル名を指定します．印刷フォーマットは拡張子によ
   り決定します．拡張子が.psの場合はPostScript形式で，.pdfの
   場合はPDF形式で出力します．それ以外の場合はPostScript形式
   で出力します．
   
   "|プログラム名"とすると，PostScript形式の出力をプログラム
   に渡します．
   
   デフォルトは"#{スライドのタイトル}.ps"です．

: --paper-width=WIDTH
   印刷時の用紙幅を指定します．用紙幅の単位はinchです．
   
   デフォルトは横置きA4の幅です．
   
: --paper-height=HEIGHT
   印刷時の用紙の高さを指定します．用紙の高さの単位はinchです．

   デフォルトは横置きA4の高さです．
   
: --paper-size=WIDTH,HEIGHT
   印刷時の用紙の高さと幅を指定します．用紙の高さと幅の単位
   はinchです．
   
   デフォルトは横置きA4です．

: --slides-per-page=SLIDES
    1ページに何枚のスライドを入れて印刷するかを指定します．
    
    デフォルトは1枚です．

: --margin={全部|上下,左右|上,左右,した|上,右,下,左}, --margin-*=MARGIN
    1ページに複数枚のスライドを印刷する時のスライドの周りの
    余白を指定します．
    
    デフォルトではスライドの枚数に応じて調節しますが，2枚と8
    枚以外の時はあまりうまくありません．

: --page-margin={全部|上下,左右|上,左右,した|上,右,下,左}, --page-margin-*=MARGIN
    印刷時のページの余白を指定します．
    
    デフォルトでは余白はとられません．

: --locale-dir=DIR
   ロケール用データ（*.mo）を置くためのディレクトリを指定し
   ます．Rabbitをシステムにインストールせずに使う場合は
   Rabbitのトップディレクトリで以下のようにします．

     % ruby -I./lib bin/rabbit --locale-dir data/locale sample/rabbit.rd
   
   デフォルトでは/usr/local/share/locale/や
   /usr/share/locale/あたりが使われます．

: --logger-type=TYPE
   エラーログをどのように出力するかを指定します．guiを指定す
   るとエラーログはダイアログボックスに表示されます．rabbit
   の起動オプションを解析するときにエラーが起こることもある
   ので，--logger-typeはオプションの先頭で指定することをお薦め
   します．

     % rabbit --logger-type gui ...
   
   デフォルトはstderrで標準エラー出力に表示されます．

: --use-druby, --no-use-druby
   dRubyインターフェイスを使うかどうかを指定します．

   デフォルトでは使います．

: --druby-uri=URI
   dRubyインターフェイスのURIを指定します．

   デフォルトではdruby://:10101です．

: --output-druby-uri, --no-output-druby-uri
   dRubyインターフェイスのURIを表示するかどうかを指定します．

   デフォルトでは表示しません．

: --use-soap, --no-use-soap
   SOAPインターフェイスを使うかどうかを指定します．

   デフォルトでは使いません．

: --soap-host=HOST
   SOAPインターフェイスのホストを指定します．

   デフォルトでは0.0.0.0です．

: --soap-port=PORT
   SOAPインターフェイスのポートを指定します．

   デフォルトでは10103です．

: --use-xmlrpc, --no-use-xmlrpc
   XML-RPCインターフェイスを使うかどうかを指定します．

   デフォルトでは使いません．

: --xmlrpc-host=HOST
   XML-RPCインターフェイスのホストを指定します．

   デフォルトでは0.0.0.0です．

: --xmlrpc-port=PORT
   XML-RPCインターフェイスのポートを指定します．

   デフォルトでは10104です．

: --server, --no-server
   サーバとして起動するかを指定します．
   
   デフォルトではサーバとして起動しません．

: --public-level=LEVEL
   外部インターフェイス（dRuby/XML-RPC/SOAP経由）にRabbitの
   機能をどのくらい公開するかを指定します．公開レベルは
   strict, move, read-size, change-size, size, read-source,
   change-source, source, allから選びます．後ろに挙げた公開
   レベルほど多くの機能を公開します．

   デフォルトではstrictです．

: --comment-source=FILE
   初期コメント用ソースのファイル名を指定します．
   
   デフォルトではRabbitが提供する初期コメント用ソースが使わ
   れます．

: --comment-encoding=ENCODING
   初期コメント用ソースのエンコーディングを指定します．

   指定しなかった場合は自動検出を試みます．

: --migemo-dictionary-search-path=PATH1,PATH2,...
   Migemoの静的辞書の検索パスを指定します．検索パスは
   --migemo-dictionary-nameで指定した静的辞書があるディレク
   トリか，静的辞書のパスを指定します．コンマで区切って複数
   のパスを指定することができます．

   デフォルトは/usr/local/share, /usr/shareです．

: --migemo-dictionary-name=NAME
   Migemoの静的辞書名を指定します．

   デフォルトはmigemo-dictです．

: --use-gl, --no-use-gl
   もし可能であればOpenGLを使うかどうかを指定します．

   デフォルトでは使いません．

: --show-native-window-id, --no-show-native-window-id
   もし可能であればウィンドウIDを表示するかどうかを指定します．

   デフォルトでは表示しません。

== ランチャーモード

コンソールがない環境でファイル名を指定せずに起動するとRabbit
はファイル選択ダイアログを表示します。ダイアログでファイルを
選択すると、Rabbitはそのスライドを表示します。
