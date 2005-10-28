= Skip up with(('&NewLine;'))Ruby-GNOME2

: place
   KOF2005ステージ

: date
   2005/10/28

: author
   須藤功平

: institution
   COZMIXNG

: theme
   ruby-gnome2

= 近況

  * 私を知っている人向け

  * Since LLDN...

    * Summer of Code((*成功！*))
      * スーパーハッカー認定 by AC
    * ((*R*))abbit ((*D*))riven ((*D*))evelopment
      * cairoのRubyバインディング作成

= self.about(('note:(Date.today)'))

  * An OSS Developer

    * Ruby-GNOME2コミッタ

    * rcairoコミッタ

  * An OSS User

    * もっとも古いRabbitユーザ

= Concerns

  * 私たちには時間が無い

  * 私にはネームバリューが無い

  * 私にはKOF経験が無い

私たちに必要なものは((*インパクト*))

そして((*スピード*))

= Road Map

  * 基礎はサクッと((*Skip*))

  * 見栄えのするネタ中心

= ネタ

  * ウィジェットを透過
  * ウィジェットをハイライト
  * ドラッグで移動
  * マウスジェスチャー
  * Layout + EventBoxのススメ

= 採用

  * (('del:ウィジェットを透過'))
  * ((*ウィジェットをハイライト*))
  * (('del:ドラッグで移動'))
  * ((*マウスジェスチャー*))
  * (('del:Layout + EventBoxのススメ'))

= ボツネタ

  * (('del:cairo/gnomeprintとscale'))
  * (('del:Rabbitの新機能紹介'))
  * (('del:cairo/GTK+でカスタムUI作成'))

= 基礎

= ウィジェット

GUIの構成要素: ((*重なっている*))

  # image
  # src = widget.svg
  # caption = ウィジェット
  # relative_width = 100

= GTK+のウィジェット

ウィジェット((*毎*))にGDK Window

  # image
  # src = gtk+-widget.svg
  # caption = GTK+のウィジェット
  # relative_width = 100

= ハイ(('&NewLine;'))ライト

= ハイライト

選択中のウィジェットを強調表示

  # image
  # src = high-light.svg
  # relative_width = 100

= 作戦

  * イベント捕捉
    * カーソルが入った／出た

  (1) 入っていたら自分で描画

  (2) 入ったら専用ウィジェット表示
      
      出たら専用ウィジェットを隠す

= 構成

: Gtk::Layout
   任意の場所に子ウィジェットを配置可能

 # image
 # src = high-light-architecture.svg
 # relative_width = 100

= 自分で描画

  * 実装は((*簡単*))(('note:．．．だけど'))
  * 最前面に描画((*できない*))

 # image
 # src = high-light-expose.svg
 # relative_width = 100

= 内外判定

入ったフラグを立てる

  # enscript ruby
  
  entered = false
  name = "enter_notify_event"
  box.signal_connect(name) do
    entered = true
    queue_draw
    false
  end

= 内外判定

入ったフラグを下げる

  # enscript ruby

  name = "leave_notify_event"
  box.signal_connect(name) do
    entered = false
    queue_draw
    false
  end

= 描画

入っていたら枠を描画

  # enscript ruby
  
  name = "expose_event"
  box.signal_connect_after(name) do
    if entered
      draw_rectangle(...)
    end
  end

デモ

= 専用ウィジェット

  * 実装は((*トリッキー*))(('note:．．．だけど'))
  * 最前面に描画((*できる*))

 # image
 # src = high-light-widget.svg
 # relative_width = 100

= ウィジェット定義

: Gtk::Misc
   Gtk::EventBoxのスーパークラス

   抽象クラス(('&RightArrow;'))newできない

   継承してtype_register

 # enscript ruby

 class HighLight < Gtk::Misc
   type_register

= 描画設定

  # enscript ruby

    def initialize
      super
      set_flags(Gtk::Widget::NO_WINDOW)
      signal_connect("expose_event") do
        draw_rectangle(...)
        false
      end
    end
  end


= 舞台設置

専用ウィジェットは((*1つで十分*))

  # enscript ruby
  
  layout = Gtk::Layout.new
  hl = HighLight.new
  layout.put(hl, 0, 0)
  hl.hide

= 内外判定

入ったら専用ウィジェットを表示

  # enscript ruby
  
  name = "enter_notify_event"
  rect.signal_connect(name) do
    x, y, w, h = rect.allocation.to_a
    layout.remove(hl)
    layout.put(hl, x, y)
    hl.set_size_request(w, h)
    hl.show
    false
  end

= 内外判定

出たら隠す

  # enscript ruby

  name = "leave_notify_event"
  rect.signal_connect(name) do
    hl.hide
    false
  end

デモ

= マウスジェスチャー

= 作戦

  * ウィジェットと処理を((*分離*))

  * ウィジェット

    * イベントとジェスチャー処理の橋渡し

  * ジェスチャー処理

    * マウスの動いた方向を判断
    * アクションの起動

= 処理の流れ

  # image
  # src = gesture-flow.svg
  # relative_height = 100

= 初期化

  # enscript ruby
  class GestureProcessor
    def initialize
      @threshold = 16
      @actions = []
      reset
    end
    def reset
      @started = false
      @motions = []
    end

= 開始

  # enscript ruby
  # クリックされたとき
    def start(x, y)
      @prev_x = @x = x
      @prev_y = @y = y
      @started = true
      @motions = []
    end

= 移動

  # enscript ruby
  # ドラッグされたとき
    def update_position(x, y)
      delta_x = x - @prev_x
      delta_y = y - @prev_y
      motion = judge(delta_x, delta_y)
      return unless motion
      @prev_x = @x = x
      @prev_y = @y = y
      return if @motions.last == motion
      @motions << motion
    end

= 判断

  # enscript ruby
  # 方向の判断
    def judge(dx, dy)
      if dx.abs > dy.abs
        dx < 0 ? "L" : "R"
      else
        dy < 0 ? "U" : "D"
      end
    end

= アクション

  # enscript ruby
  # アクションの追加／取得
    def add_action(seq, act=Proc.new)
      @actions << [seq, act]
    end
    def action
      @actions.each do |seq, act|
        return act if seq == @motions
      end
      nil
    end

= 実行

  # enscript ruby
  # アクションを実行
    def perform
      act = action
      act.call if act
      reset
    end
    def cancel
      reset
    end
  end

= 使用例

  # enscript ruby
  gesture.add_action(["L", "R"]) do
    puts "LEFT-RIGHT!"
  end

デモ

= おさらい（今日のネタ）

  * (('del:ウィジェットを透過'))
  * ((*ウィジェットをハイライト*))
  * (('del:ドラッグで移動'))
  * ((*マウスジェスチャー*))
  * ((*(('del:Layout + EventBoxのススメ'))*))

= まとめ

  * カスタムUIのススメ
    * Gtk::Layout
    * Gtk::EventBox (with NO_WINDOW)

  * ((*R*))abbit ((*D*))riven ((*D*))evelopment
    * cairoのススメ
    * 半透明でキレイに見える

= ((' '))

  # image
  # src = ruby-pink-circle-logo.png
  # relative_height = 100

=end

= 透過(('&NewLine;'))(('note:（ボツネタ）'))

= ウィジェットを透過

((*後ろ*))のウィジェットを見せる

  # image
  # src = mask.svg
  # caption = ウィジェットを透過
  # relative_width = 100

= マスクの初期化

描画するピクセルのbitを立てる

  # enscript ruby
  # マスク用バッファ
  mask = Gdk::Pixmap.new(nil, w, h, 1)

  # 全て描画
  set_gc = Gdk::GC.new(mask)
  set_gc.function = Gdk::GC::SET
  args = [set_gc, true, 0, 0, w, h]
  mask.draw_rectangle(*args)

= マスクの作成

描画しないピクセルのbitを反転

  # enscript ruby
  # 描画しない領域を設定
  xor_gc = Gdk::GC.new(mask)
  xor_gc.function = Gdk::GC::INVERT
  args = [xor_gc, true, 0, 0,
          w, h, 0, 360 * 64]
  mask.draw_arc(*args)

= マスクの適用

  # enscript ruby
  widget.shape_combine_mask(mask, 0, 0)

まるく穴が開く
#(('note:はずだったんだけどなぁ．'))

  # image
  # src = mask-screenshot.png
  # caption = 実行結果
  # relative_height = 100
