module Rabbit
  module Gesture
    class Processor
      DEFAULT_THRESHOLD = 48
      DEFAULT_SKEW_THRESHOLD_ANGLE = 90 / 4

      attr_accessor :threshold, :skew_threshold_angle
      attr_reader :motions

      def initialize(threshold=nil, skew_threshold_angle=nil)
        @threshold = threshold || DEFAULT_THRESHOLD
        @skew_threshold_angle = skew_threshold_angle
        @skew_threshold_angle ||= DEFAULT_SKEW_THRESHOLD_ANGLE
        reset
      end

      def started?
        @started
      end

      MOTIONS = %w(L R U D UL UR LL LR)

      def available_motion?(motion)
        MOTIONS.include?(motion)
      end

      def start(x, y)
        @prev_x = @x = x
        @prev_y = @y = y
        @started = true
        @motions = []
      end

      def update_position(x, y)
        mx = x - @prev_x
        my = y - @prev_y

        motion = judge_motion(mx, my)
        if motion
          @prev_x = @x = x
          @prev_y = @y = y
          if @motions.last == motion
            false
          else
            @motions << motion
            true
          end
        else
          false
        end
      end

      def reset
        @started = false
        @x = @y = -1
        @motions = []
      end

      def to_a
        @motions
      end

      def position
        [@x, @y]
      end

      private
      def judge_motion(dx, dy)
        dxa = dx.abs
        dya = dy.abs
        distance = Math.sqrt(dxa ** 2 + dya ** 2)
        upper_theta = (45 + @skew_threshold_angle) * (Math::PI / 180.0)
        lower_theta = (45 - @skew_threshold_angle) * (Math::PI / 180.0)
        if distance > @threshold and
            dya < Math.tan(upper_theta) * dxa and
            dya > Math.tan(lower_theta) * dxa
          judge_corner_motion(dx, dy)
        elsif dxa > @threshold or dya > @threshold
          judge_cross_motion(dx, dy)
        else
          nil
        end
      end

      def judge_corner_motion(dx, dy)
        if dx < 0
          if dy < 0
            "UL"
          else
            "LL"
          end
        else
          if dy < 0
            "UR"
          else
            "LR"
          end
        end
      end

      def judge_cross_motion(dx, dy)
        if dx.abs > dy.abs
          if dx < 0
            "L"
          else
            "R"
          end
        else
          if dy < 0
            "U"
          else
            "D"
          end
        end
      end
    end
  end
end
