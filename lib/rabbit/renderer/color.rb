require "rabbit/gtk"

require "rabbit/rabbit"

module Rabbit
  module Renderer
    class Color < Struct.new(:red, :green, :blue, :alpha)
      class << self
        def parse(*args)
          new(*args)
        end
      end

      def initialize(*args)
        case args.collect(&:class)
        when [Gdk::RGBA]
          rgba = args[0]
          super(*rgba.to_a)
        when [String]
          rgba = parse(args[0])
          super(*rgba)
        else
          super(*args)
        end
      end

      def to_s
        "#%04X%04X%04X%04X" % to_a
      end

      def to_a
        [red, green, blue, alpha]
      end

      def to_gdk_rgba
        Gdk::RGBA.new(*to_a)
      end

      def to_gdk_format
        to_s
      end

      def to_css_rgba
        red_percent = (red * 100).ceil
        green_percent = (green * 100).ceil
        blue_percent = (blue * 100).ceil
        a = alpha || 1.0
        "rgba(#{red_percent}%, #{green_percent}%, #{blue_percent}%, #{a})"
      end

      private
      HEX = "(?i:[a-z0-9])"
      def parse(text)
        case text
        when /\A\#(#{HEX})(#{HEX})(#{HEX})(#{HEX})?\z/
          normalize_rgba($1, $2, $3, $4, 16 ** 1 - 1)
        when /\A\#(#{HEX}{2})(#{HEX}{2})(#{HEX}{2})(#{HEX}{2})?\z/
          normalize_rgba($1, $2, $3, $4, 16 ** 2 - 1)
        when /\A\#(#{HEX}{4})(#{HEX}{4})(#{HEX}{4})(#{HEX}{4})?\z/
          normalize_rgba($1, $2, $3, $4, 16 ** 4 - 1)
        else
          Gdk::RGBA.parse(text).to_a
        end
      end

      def normalize_rgba(r, g, b, a, max)
        red = r.hex / max.to_f
        green = g.hex / max.to_f
        blue = b.hex / max.to_f
        if a
          alpha = a.hex / max.to_f
        else
          alpha = 1.0
        end
        [red, green, blue, alpha]
      end
    end
  end
end
