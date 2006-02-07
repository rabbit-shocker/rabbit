require 'gtk2'

require 'rabbit/rabbit'
require 'rabbit/gesture/processor'
require 'rabbit/renderer/color'

module Rabbit
  module Gesture
    class Handler
      DEFAULT_BACK_COLOR = Renderer::Color.parse("#3333337f")
      DEFAULT_LINE_COLOR = Renderer::Color.parse("#f00f")
      DEFAULT_NEXT_COLOR = Renderer::Color.parse("#0f0c")
      DEFAULT_CURRENT_COLOR = Renderer::Color.parse("#f0fc")

      attr_accessor :back_color, :line_color, :next_color, :current_color
      def initialize(conf={})
        super()
        conf ||= {}
        @back_color = conf[:back_color] || DEFAULT_BACK_COLOR
        @line_color = conf[:line_color] || DEFAULT_LINE_COLOR
        @next_color = conf[:next_color] || DEFAULT_NEXT_COLOR
        @current_color = conf[:current_color] || DEFAULT_CURRENT_COLOR
        @processor = Processor.new(conf[:threshold],
                                   conf[:skew_threshold_angle])
        @actions = []
        @locus = []
      end

      def processing?
        @processor.started?
      end

      def clear_actions
        @actions.clear
      end

      def add_action(sequence, action, &block)
        invalid_motion = sequence.find do |motion|
          not @processor.available_motion?(motion)
        end
        raise InvalidMotionError.new(invalid_motion) if invalid_motion
        @actions << [sequence, action, block]
      end

      def start(button, x, y, base_x, base_y)
        @button = button
        @processor.start(x, y)
        @base_x = base_x
        @base_y = base_y
        @locus = [[x, y]]
      end

      def button_release(x, y, width, height)
        perform_action
      end

      def button_motion(x, y, width, height)
        new_x = @base_x + x
        new_y = @base_y + y
        @locus << [new_x, new_y]
        @processor.update_position(new_x, new_y)
      end

      def draw_last_locus(drawable)
        return unless drawable.respond_to?(:create_cairo_context)
        if @locus.size >= 2
          cr = drawable.create_cairo_context
          cr.set_source_rgba(@line_color.to_a)
          cr.move_to(*@locus[-2])
          cr.line_to(*@locus[-1])
          cr.stroke
        end
      end

      def draw(drawable)
        return unless drawable.respond_to?(:create_cairo_context)
        cr = drawable.create_cairo_context

        cr.rectangle(0, 0, *drawable.size)
        cr.set_source_rgba(@back_color.to_a)
        cr.fill

        cr.set_source_rgba(@next_color.to_a)
        draw_available_marks(cr, next_available_motions)

        act, = action
        if act
          cr.set_source_rgba(@current_color.to_a)
          draw_mark(cr, act, *@processor.position)
        end

        draw_locus(drawable)
      end

      def draw_locus(drawable)
        return unless drawable.respond_to?(:create_cairo_context)
        return if @locus.empty?
        cr = drawable.create_cairo_context
        cr.set_source_rgba(@line_color.to_a)
        first, *rest = @locus
        cr.move_to(*first)
        rest.each do |locus|
          cr.line_to(*locus)
        end
        cr.stroke
      end

      private
      def perform_action
        act, block = action
        @processor.reset
        @locus.clear
        if act
          act.activate(&block)
          true
        else
          false
        end
      end

      def action
        motions = @processor.motions
        @actions.each do |sequence, act, block|
          return [act, block] if sequence == motions and act.sensitive?
        end
        nil
      end

      def available_motions
        motions = @processor.motions
        @actions.collect do |sequence, act, block|
          if sequence == motions and act.sensitive?
            [sequence.last, act]
          else
            nil
          end
        end.compact.uniq
      end

      def next_available_motions
        motions = @processor.motions
        @actions.collect do |sequence, act, block|
          if sequence[0..-2] == motions and act.sensitive?
            [sequence.last, act]
          else
            nil
          end
        end.compact.uniq
      end

      def match?
        not action.nil?
      end

      def draw_mark(cr, act, x=nil, y=nil, radius=nil)
        x ||= @processor.position[0]
        y ||= @processor.position[1]
        radius ||= @processor.threshold
        cr.save do
          cr.arc(x, y, radius / 2, 0, 2 * Math::PI)
          cr.fill
        end
        draw_action_image(cr, act, x, y)
      end

      def draw_action_image(cr, act, x, y)
        icon = nil
        icon = act.create_icon(Gtk::IconSize::DIALOG) if act
        if icon
          pixbuf = icon.render_icon(icon.stock, icon.icon_size, act.name)
          cr.save do
            cr.translate(x - pixbuf.width / 2.0, y - pixbuf.height / 2.0)
            cr.set_source_pixbuf(pixbuf, 0, 0)
            cr.paint
          end
        elsif block_given?
          yield
        end
      end

      def draw_available_marks(cr, infos)
        infos.each do |motion, act|
          args = [motion, %w(R), %w(L), %w(UR LR), %w(UL LL)]
          adjust_x = calc_position_ratio(*args)
          args = [motion, %w(D), %w(U), %w(LR LL), %w(UR UL)]
          adjust_y = calc_position_ratio(*args)

          threshold = @processor.threshold
          x, y = @processor.position
          center_x = x + threshold * adjust_x
          center_y = y + threshold * adjust_y
          draw_action_image(cr, act, center_x, center_y) do
            cr.save do
              cr.set_line_width(10)
              cr.translate(x, y)
              angle = @processor.skew_threshold_angle * 2 * (Math::PI / 180.0)
              base_angle = -angle / 2
              adjust_angle = calc_position_angle(motion)
              cr.rotate(base_angle + adjust_angle)
              cr.arc(0, 0, threshold, 0, angle)
              cr.stroke
            end
          end
        end
      end

      def calc_position_ratio(motion, inc, dec, inc_skew, dec_skew)
        case motion
        when *inc
          1
        when *inc_skew
          1 / Math.sqrt(2)
        when *dec
          -1
        when *dec_skew
          -1 / Math.sqrt(2)
        else
          0
        end
      end

      MOTION_TO_ANGLE = {
        "R" => 0,
        "LR" => Math::PI / 4,
        "D" => Math::PI / 2,
        "LL" => Math::PI / 1,
        "L" => Math::PI,
        "UL" => Math::PI * 5 / 4,
        "U" => Math::PI * 3/ 2,
        "UR" => Math::PI * 7 / 4,
      }
      def calc_position_angle(motion)
        MOTION_TO_ANGLE[motion]
      end
    end
  end
end
