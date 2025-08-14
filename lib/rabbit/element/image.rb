# Copyright (C) 2004-2021  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../image"
require_relative "block-element"

module Rabbit
  module Element
    class Image
      include Base
      include BlockElement
      include BlockHorizontalCentering

      alias element_draw draw
      include ImageManipulable
      alias image_draw draw
      alias draw element_draw

      def initialize(filename, props, canvas: nil)
        super(filename, props, canvas: canvas)
        setup_draw_parameters
        resize(properties.get_size("width", @filename),
               properties.get_size("height", @filename))
      end

      def draw_element(canvas, x, y, w, h, simulation)
        result = draw_image(canvas, x, y, w, h, simulation)
        draw_properties(canvas, x, y, w, h) unless simulation
        result
      end

      def caption
        self["caption"]
      end

      def caption_font_size
        properties.get_float("caption-font-size")
      end

      def text
        caption.to_s
      end

      def to_rd
        text
      end

      def to_html(generator)
        src = generator.save_pixbuf(pixbuf, File.basename(@filename))
        html = "<img "
        if @caption
          alt = generator.h(@caption)
          html << "title=\"#{alt}\" alt=\"#{alt}\" "
        end
        html << "src=\"#{src}\" />"
        html
      end

      def relative_width
        properties.get_relative_size("relative_width", @filename)
      end

      def relative_height
        properties.get_relative_size("relative_height", @filename)
      end

      def relative_margin_top
        properties.get_relative_size("relative_margin_top", @filename)
      end

      def relative_margin_bottom
        properties.get_relative_size("relative_margin_bottom", @filename)
      end

      def relative_margin_left
        properties.get_relative_size("relative_margin_left", @filename)
      end

      def relative_margin_right
        properties.get_relative_size("relative_margin_right", @filename)
      end

      def relative_padding_top
        properties.get_relative_size("relative_padding_top", @filename)
      end

      def relative_padding_bottom
        properties.get_relative_size("relative_padding_bottom", @filename)
      end

      def relative_padding_left
        properties.get_relative_size("relative_padding_left", @filename)
      end

      def relative_padding_right
        properties.get_relative_size("relative_padding_right", @filename)
      end

      alias _compile compile
      def compile_for_horizontal_centering(canvas, x, y, w, h)
        _compile(canvas, x, y, w, h)
      end

      def compile(canvas, x, y, w, h)
        super
        adjust_size(canvas, @x, @y, @w, @h)
      end

      alias_method :width_without_padding, :width
      def width
        super + @padding_left + @padding_right
      end

      alias_method :height_without_padding, :height
      def height
        super + @padding_top + @padding_bottom
      end

      def as_large_as_possible?
        properties.get_boolean("as_large_as_possible")
      end

      def setup_scene_element(canvas, scene_widget, x, y, w, h)
        x, y, w, h = super
        if @loader.animation and not @loader.animation.static_image?
          texture = Gdk::PixbufAnimationPaintable.new(@loader.animation)
        else
          texture = Gdk::Texture.new(@loader.pixbuf)
        end
        picture_widget = Gtk::Picture.new(texture)
        scene_widget.put(picture_widget, x, y, @loader.width, @loader.height)
        y += @loader.height
        h -= @loader.height

        [x, y, w, h]
      end

      def scene_snapshot_element(widget, snapshot, canvas, x, y, w, h)
        y += @loader.height
        h -= @loader.height
        [x, y, w, h]
      end

      private
      def setup_draw_parameters
        @draw_parameters = {}

        @draw_parameters[:reflect] = {} if properties.get_boolean("reflect")
        [:ratio, :alpha].each do |key|
          name = "reflect_#{key}"
          value = self[name]
          next unless value
          @draw_parameters[:reflect] ||= {}
          @draw_parameters[:reflect][key] = Float(value)
        end

        alpha = self["alpha"]
        @draw_parameters[:alpha] = Float(alpha) if alpha
      end

      def draw_image(canvas, x, y, w, h, simulation)
        unless simulation
          image_draw(canvas, x, y, @draw_parameters)
        end
        [x, y + height, w, h - height]
      end

      def adjust_margin(w, h)
        @margin_top =
          relative_margin_top&.resolve(h) || @margin_top
        @margin_bottom =
          relative_margin_bottom&.resolve(h) || @margin_bottom
        @margin_left =
          relative_margin_left&.resolve(w) || @margin_left
        @margin_right =
          relative_margin_right&.resolve(w) || @margin_right
      end

      def adjust_padding(w, h)
        @padding_top =
          relative_padding_top&.resolve(h) || @padding_top
        @padding_bottom =
          relative_padding_bottom&.resolve(h) || @padding_bottom
        @padding_left =
          relative_padding_left&.resolve(w) || @padding_left
        @padding_right =
          relative_padding_right&.resolve(w) || @padding_right
      end

      def adjust_size(canvas, x, y, w, h)
        base_w = w
        base_h = @oh || h
        adjust_margin(base_w, base_h)
        adjust_padding(base_w, base_h)
        base_h = base_h - @padding_top - @padding_bottom
        if as_large_as_possible?
          iw = base_w - x
          ih = base_h - y
          if iw.to_f / original_width > ih.to_f / original_height
            iw = nil
          else
            ih = nil
          end
        else
          iw = relative_width&.resolve(base_w)
          ih = relative_height&.resolve(base_h)
        end
        resize(iw, ih)
      end

      def draw_properties(canvas, base_x, base_y, base_w, base_h)
        properties.draws.each do |type, *args|
          case type
          when "line"
            if args.last.is_a?(Hash)
              params = args.pop
            else
              params = nil
            end
            points = args.each_slice(2).collect do |x, y|
              x = (x * width_without_padding) + base_x
              y = (y * height_without_padding) + base_y
              [x, y]
            end
            params = normalize_params(params)
            color = params.delete(:color) || "black"
            canvas.draw_lines(points, color, params)
          when "rectangle"
            filled, x, y, w, h, params = args
            x = (x * width_without_padding) + base_x
            y = (y * height_without_padding) + base_y
            w = (w * width_without_padding)
            h = (h * height_without_padding)
            params = normalize_params(params)
            color = params.delete(:color) || "black"
            canvas.draw_rectangle(filled, x, y, w, h, color, params)
          when "text"
            text, x, y, params = args
            params = normalize_params(params)
            layout = canvas.make_layout(markup_text(text, params))
            x = (x * width_without_padding) + base_x
            y = (y * height_without_padding) + base_y
            color = params.delete(:color) || "black"
            canvas.draw_layout(layout, x, y, color, params)
          end
        end
      end

      def normalize_params(params)
        return {} if params.nil?
        normalized_params = {}
        params.each do |key, value|
          normalized_params[key.to_sym] = value
        end
        normalized_params
      end

      def markup_text(text, props)
        props.each do |name, value|
          formatter_name = Utils.to_class_name(name.to_s)
          next unless Format.const_defined?(formatter_name)
          formatter = Format.const_get(formatter_name).new(value)
          next unless formatter.text_formatter?
          text = formatter.format(text)
        end
        text
      end
    end
  end
end
