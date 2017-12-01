# Copyright (C) 2006-2017  Kouhei Sutou <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require 'rabbit/gtk'

require 'rabbit/rabbit'
require 'rabbit/renderer/engine'
require 'rabbit/gesture/processor'

module Rabbit
  module Gesture
    class Handler
      DEFAULT_BACK_COLOR = Renderer::Color.parse("#3333337f")
      DEFAULT_LINE_COLOR = Renderer::Color.parse("#f00f")
      DEFAULT_NEXT_COLOR = Renderer::Color.parse("#0f0c")
      DEFAULT_CURRENT_COLOR = Renderer::Color.parse("#f0fc")

      DEFAULT_LINE_WIDTH = 3
      DEFAULT_NEXT_WIDTH = 10

      attr_accessor :back_color, :line_color, :next_color, :current_color
      attr_accessor :line_width, :next_width
      def initialize(conf={})
        super()
        conf ||= {}
        @back_color = conf[:back_color] || DEFAULT_BACK_COLOR
        @line_color = conf[:line_color] || DEFAULT_LINE_COLOR
        @next_color = conf[:next_color] || DEFAULT_NEXT_COLOR
        @current_color = conf[:current_color] || DEFAULT_CURRENT_COLOR
        @line_width = conf[:line_width] || DEFAULT_LINE_WIDTH
        @next_width = conf[:next_width] || DEFAULT_NEXT_WIDTH
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

      def draw_last_locus(renderer)
        if @locus.size >= 2
          x1, y1 = @locus[-2]
          x2, y2 = @locus[-1]
          args = [x1, y1, x2, y2, @line_color, {:line_width => @line_width}]
          renderer.draw_line(*args)
        end
      end

      def draw(renderer)
        if @back_color.alpha == 1.0 or
            (@back_color.alpha < 1.0 and renderer.alpha_available?)
          size = renderer.size
          args = [true, 0, 0, size.real_width, size.real_height]
          args << @back_color
          renderer.draw_rectangle(*args)
        end

        draw_available_marks(renderer, next_available_motions)

        act, = action
        draw_mark(renderer, act, *@processor.position) if act

        draw_locus(renderer)
      end

      def draw_locus(renderer)
        return if @locus.empty?
        renderer.draw_lines(@locus, @line_color, {:line_width => @line_width})
      end

      def moved?
        @locus.size >= 2
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

      def draw_mark(renderer, act, x=nil, y=nil, radius=nil)
        x ||= @processor.position[0]
        y ||= @processor.position[1]
        radius ||= @processor.threshold / 2.0
        renderer.draw_circle_by_radius(true, x, y, radius, @current_color)
        draw_action_image(renderer, act, x, y)
      end

      def draw_action_image(renderer, act, x, y)
        icon = nil
        icon = act.create_icon(Gtk::IconSize::DIALOG) if act
        if icon
          stock, = icon.stock
          icon_size = icon.icon_size
          pixbuf = icon.render_icon_pixbuf(stock, icon_size)
          x -= pixbuf.width / 2.0
          y -= pixbuf.height / 2.0
          renderer.draw_pixbuf(pixbuf, x, y)
        elsif block_given?
          yield
        end
      end

      def draw_available_marks(renderer, infos)
        infos.each do |motion, act|
          args = [motion, %w(R), %w(L), %w(UR LR), %w(UL LL)]
          adjust_x = calc_position_ratio(*args)
          args = [motion, %w(D), %w(U), %w(LR LL), %w(UR UL)]
          adjust_y = calc_position_ratio(*args)

          threshold = @processor.threshold
          x, y = @processor.position
          center_x = x + threshold * adjust_x
          center_y = y + threshold * adjust_y
          draw_action_image(renderer, act, center_x, center_y) do
            angle = @processor.skew_threshold_angle
            base_angle = calc_position_angle(motion) - angle
            args = [false, x, y, threshold, base_angle, angle * 2]
            args.concat([@next_color, {:line_width => @next_width}])
            renderer.draw_arc_by_radius(*args)
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
        "UR" => 45,
        "U" => 90,
        "UL" => 135,
        "L" => 180,
        "LL" => 225,
        "D" => 270,
        "LR" => 315,
      }
      def calc_position_angle(motion)
        MOTION_TO_ANGLE[motion]
      end

      def make_color(color)
        color ||= @back_color
        if color.is_a?(Renderer::Color)
          color
        else
          Renderer::Color.parse(color)
        end
      end
    end
  end
end
