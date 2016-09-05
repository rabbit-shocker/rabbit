require "rabbit/gtk"

require "rabbit/rabbit"

module Rabbit
  module Renderer
    Color = Struct.new(:red, :green, :blue, :alpha, :have_alpha)
    class Color
      GDK_COLOR_NORMALIZE = 65535.0

      class << self
        def new_from_gdk_color(color, have_alpha=false)
          red = color.red / GDK_COLOR_NORMALIZE
          green = color.green / GDK_COLOR_NORMALIZE
          blue = color.blue / GDK_COLOR_NORMALIZE
          alpha = 1.0
          new(red, green, blue, alpha, have_alpha)
        end

        HEX = "(?i:[a-z0-9])"
        def parse(spec)
          case spec
          when Array
            new(*spec)
          when /\A\#(#{HEX})(#{HEX})(#{HEX})(#{HEX})?\z/
            new(*normalize($1, $2, $3, $4, 16 ** 1 - 1))
          when /\A\#(#{HEX}{2})(#{HEX}{2})(#{HEX}{2})(#{HEX}{2})?\z/
            new(*normalize($1, $2, $3, $4, 16 ** 2 - 1))
          when /\A\#(#{HEX}{4})(#{HEX}{4})(#{HEX}{4})(#{HEX}{4})?\z/
            new(*normalize($1, $2, $3, $4, 16 ** 4 - 1))
          else
            new_from_gdk_color(Gdk::Color.parse(spec))
          end
        end

        def normalize(r, g, b, a, max)
          red = r.hex / max.to_f
          green = g.hex / max.to_f
          blue = b.hex / max.to_f
          have_alpha = !a.nil?
          alpha = have_alpha ? a.hex / max.to_f : 1.0
          [red, green, blue, alpha, have_alpha]
        end
      end

      alias have_alpha? have_alpha

      def to_s
        "#%04X%04X%04X%04X" % to_gdk_rgba
      end

      def to_a
        if have_alpha?
          [red, green, blue, alpha]
        else
          [red, green, blue]
        end
      end

      def to_gdk_rgb
        to_gdk_rgba[0..-2]
      end

      def to_gdk_rgba
        [red, green, blue, alpha || 1.0].collect do |color|
          (color * GDK_COLOR_NORMALIZE).truncate
        end
      end

      def to_gdk_format
        to_s.gsub(/[a-z0-9]{4}\z/i, '')
      end

      def to_gdk_color
        Gdk::Color.new(*to_gdk_rgb)
      end

      def to_css_rgba
        red_percent = (red * 100).ceil
        green_percent = (green * 100).ceil
        blue_percent = (blue * 100).ceil
        a = alpha || 1.0
        "rgba(#{red_percent}%, #{green_percent}%, #{blue_percent}%, #{a})"
      end
    end
  end
end
