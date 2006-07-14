require 'cairo'

require 'rabbit/renderer/kernel'

module Cairo
  class Context
    unless instance_methods.include?("rounded_rectangle")
      def rounded_rectangle(x, y, width, height, x_radius, y_radius=nil)
        x1 = x
        x2 = x1 + width
        y1 = y
        y2 = y1 + height

        y_radius ||= x_radius

        x_radius = [x_radius, width / 2].min
        y_radius = [y_radius, height / 2].min

        xr1 = x_radius
        xr2 = x_radius / 2.0
        yr1 = y_radius
        yr2 = y_radius / 2.0

        move_to(x1 + xr1, y1)
        line_to(x2 - xr1, y1)
        curve_to(x2 - xr2, y1, x2, y1 + yr2, x2, y1 + yr1)
        line_to(x2, y2 - yr1)
        curve_to(x2, y2 - yr2, x2 - xr2, y2, x2 - xr1, y2)
        line_to(x1 + xr1, y2)
        curve_to(x1 + xr2, y2, x1, y2 - yr2, x1, y2 - yr1)
        line_to(x1, y1 + yr1)
        curve_to(x1, y1 + yr2, x1 + xr2, y1, x1 + xr1, y1)
        close_path
      end
    end
  end
end

module Rabbit
  module Renderer
    module Engine
      module Cairo
        include Kernel

        @@rsvg_available = nil
        @@poppler_available = nil

        class << self
          def available_with_gdk?
            Gdk.respond_to?(:cairo_available?) and Gdk.cairo_available?
          end

          def available_with_pango?
            Pango.respond_to?(:cairo_available?) and Pango.cairo_available?
          end

          def priority
            if available_with_gdk? and available_with_pango?
              100
            else
              -100
            end
          end
        end

        attr_writer :foreground, :background, :background_image

        def alpha_available?
          true
        end

        def init_renderer(drawable)
          @context = drawable.create_cairo_context
          init_context
        end

        def init_context
          set_line_width(1)
          @context.line_cap = ::Cairo::LINE_CAP_ROUND
          @context.line_join = ::Cairo::LINE_JOIN_ROUND
        end

        def to_gdk_rgb(color)
          make_color(color).to_gdk_rgb
        end

        def draw_line(x1, y1, x2, y2, color=nil, params={})
          x1, y1 = from_screen(x1, y1)
          x2, y2 = from_screen(x2, y2)
          color = make_color(color)
          @context.save do
            set_color(color)
            set_line_width(get_line_width(params))
            @context.stroke do
              @context.move_to(x1, y1)
              @context.line_to(x2, y2)
            end
          end
        end

        def draw_rectangle(filled, x, y, w, h, color=nil, params={})
          x, y = from_screen(x, y)
          color = make_color(color)
          @context.save do
            set_color(color)
            set_line_width(get_line_width(params))
            @context.rectangle(x, y, w, h)
            if filled
              @context.fill
            else
              @context.stroke
            end
          end
        end

        def draw_rounded_rectangle(filled, x, y, w, h, radius,
                                   color=nil, params={})
          x, y = from_screen(x, y)
          x_radius = params[:x_radius] || radius
          y_radius = params[:y_radius] || radius

          @context.save do
            set_color(make_color(color))
            set_line_width(get_line_width(params))
            @context.new_path
            @context.rounded_rectangle(x, y, w, h, x_radius, y_radius)
            if filled
              @context.fill
            else
              @context.stroke
            end
          end
        end

        def draw_arc(filled, x, y, w, h, a1, a2, color=nil, params={})
          r = w * 0.5
          draw_arc_by_radius(filled, x + w * 0.5, y + h * 0.5,
                             r, a1, a2, color, params)
        end

        def draw_arc_by_radius(filled, x, y, r, a1, a2, color=nil, params={})
          x, y = from_screen(x, y)
          a1, a2 = convert_angle(a1, a2)
          color = make_color(color)
          @context.save do
            set_color(color)
            set_line_width(get_line_width(params))
            args = [x, y, r, a1, a2]
            if filled
              @context.fill do
                @context.move_to(x, y)
                @context.arc(*args)
                @context.close_path
              end
            else
              @context.stroke do
                @context.arc(*args)
              end
            end
          end
        end

        def draw_lines(points, color=nil, params={})
          draw_polygon(false, points, color, params.merge({:opened => true}))
        end

        def draw_polygon(filled, points, color=nil, params={})
          return if points.empty?
          color = make_color(color)
          @context.save do
            set_color(color)
            set_line_width(get_line_width(params))
            @context.move_to(*from_screen(*points.first))
            points[1..-1].each do |x, y|
              @context.line_to(*from_screen(x, y))
            end
            @context.line_to(*from_screen(*points.first)) unless params[:opened]
            if filled
              @context.fill
            else
              @context.stroke
            end
          end
        end

        def draw_layout(layout, x, y, color=nil, params={})
          x, y = from_screen(x, y)
          color = make_color(color)
          @context.save do
            set_color(color)
            set_line_width(get_line_width(params))
            @context.move_to(x, y)
            @context.show_pango_layout(layout)
          end
        end

        def draw_pixbuf(pixbuf, x, y, params={})
          x, y = from_screen(x, y)
          @context.save do
            @context.translate(x, y)
            @context.set_source_pixbuf(pixbuf, 0, 0)
            @context.paint
          end
        end

        def rsvg_available?
          if @@rsvg_available.nil?
            instance_methods = ::Cairo::Context.instance_methods
            @@rsvg_available = instance_methods.include?("render_rsvg_handle")
          end
          @@rsvg_available
        end

        def draw_rsvg_handle(handle, x, y, params={})
          x, y = from_screen(x, y)
          dim = handle.dimensions
          width = (params["width"] || dim.width).to_f
          height = (params["height"] || dim.height).to_f
          @context.save do
            @context.translate(x, y)
            @context.scale(width / dim.width, height / dim.height)
            @context.render_rsvg_handle(handle)
          end
        end

        def poppler_available?
          if @@poppler_available.nil?
            instance_methods = ::Cairo::Context.instance_methods
            available = instance_methods.include?("render_poppler_page")
            @@poppler_available = available
          end
          @@poppler_available
        end

        def draw_poppler_page(page, x, y, params={})
          x, y = from_screen(x, y)
          w, h = page.size
          width = (params["width"] || w).to_f
          height = (params["height"] || h).to_f
          @context.save do
            @context.translate(x, y)
            @context.scale(width / w, height / h)
            @context.render_poppler_page(page)
          end
        end

        def make_layout(text)
          attrs, text = Pango.parse_markup(text)
          layout = @context.create_pango_layout
          layout.text = text
          layout.set_attributes(attrs)
          layout.context.resolution = @x_dpi
          @context.update_pango_layout(layout)
          layout
        end

        private
        def init_engine_color
          @foreground = make_color("black")
          @background = make_color(@background_color)
        end

        def set_color(color)
          @context.set_source_rgba(color.to_a)
        end

        def set_line_width(line_width)
          if line_width
            @context.set_line_width(line_width)
          end
        end

        def convert_angle(a1, a2)
          a2 += a1
          [a2, a1].collect {|a| (360 - a) * (Math::PI / 180.0)}
        end

        def from_screen(x, y)
          [x.ceil, y.ceil]
        end
      end
    end
  end
end
