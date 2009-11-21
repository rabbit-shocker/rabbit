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

          def available_with_gdk_pixbuf?
            Gdk::Pixbuf.respond_to?(:cairo_available?) and
              Gdk::Pixbuf.cairo_available?
          end

          def available_with_pango?
            Pango.respond_to?(:cairo_available?) and Pango.cairo_available?
          end

          def priority
            if available_with_gdk? and available_with_gdk_pixbuf? and
                available_with_pango?
              100
            else
              -100
            end
          end
        end

        attr_writer :foreground, :background

        def alpha_available?
          true
        end

        def background_image=(pixbuf)
          surface = ::Cairo::ImageSurface.new(::Cairo::FORMAT_A1, 1, 1)
          context = ::Cairo::Context.new(surface)
          context.set_source_pixbuf(pixbuf)
          @background_image = context.source
          @background_image.extend = ::Cairo::EXTEND_REPEAT
          pixbuf
        end

        def prepare_renderer(drawable)
        end

        def init_renderer(drawable)
          init_context(drawable.create_cairo_context)
        end

        def finish_renderer
          finish_context
        end

        def init_context(context)
          @context = context
          @contexts ||= []
          @contexts.push(@context)
          set_line_width(1)
          @context.line_cap = ::Cairo::LINE_CAP_ROUND
          @context.line_join = ::Cairo::LINE_JOIN_ROUND
        end

        def finish_context
          @contexts.pop
          @context.destroy if @context.respond_to?(:destroy)
          @context = @contexts.last
        end

        def to_gdk_rgb(color)
          make_color(color).to_gdk_rgb
        end

        def translate_context(x, y, params={})
          @context.translate(x, y)
        end

        def rotate_context(angle, params={})
          @context.rotate(convert_angle(angle, 0)[0])
        end

        def scale_context(x, y, params={})
          @context.scale(x, y)
        end

        def reflect_context(base, params={})
          case base
          when :y
            matrix = make_matrix(-1, 0, 0,
                                  0, 1, 0)
          else
            matrix = make_matrix(1,  0, 0,
                                 0, -1, 0)
          end
          @context.transform(matrix)
        end

        def shear_context(x, y, params={})
          @context.transform(make_matrix(1, x, 0,
                                         y, 1, 0))
        end

        def save_context
          @context.save
          super
        end

        def restore_context
          @context.restore
        end

        def draw_background(slide)
          super
          if @background_image
            @context.save do
              @context.set_source(@background_image)
              @context.paint
            end
          end
        end

        def draw_line(x1, y1, x2, y2, color=nil, params={})
          x1, y1 = from_screen(x1, y1)
          x2, y2 = from_screen(x2, y2)
          @context.save do
            set_source(color, params)
            set_line_width(get_line_width(params))
            @context.new_path
            @context.move_to(x1, y1)
            @context.line_to(x2, y2)
            apply_cairo_action(false, params)
          end
        end

        def draw_rectangle(filled, x, y, w, h, color=nil, params={})
          x, y = from_screen(x, y)
          @context.save do
            set_source(color, params)
            set_line_width(get_line_width(params))
            @context.rectangle(x, y, w, h)
            apply_cairo_action(filled, params)
          end
        end

        def draw_rounded_rectangle(filled, x, y, w, h, radius,
                                   color=nil, params={})
          x, y = from_screen(x, y)
          x_radius = params[:x_radius] || radius
          y_radius = params[:y_radius] || radius

          @context.save do
            set_source(color, params)
            set_line_width(get_line_width(params))
            @context.new_path
            @context.rounded_rectangle(x, y, w, h, x_radius, y_radius)
            apply_cairo_action(filled, params)
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
          @context.save do
            set_source(color, params)
            set_line_width(get_line_width(params))
            args = [x, y, r, a1, a2]
            action, = cairo_action(filled, params)
            @context.move_to(x, y) unless action == :stroke
            @context.arc(*args)
            @context.close_path unless action == :stroke
            apply_cairo_action(filled, params)
          end
        end

        def draw_lines(points, color=nil, params={})
          draw_polygon(false, points, color, params.merge({:opened => true}))
        end

        def draw_polygon(filled, points, color=nil, params={})
          return if points.empty?
          @context.save do
            set_source(color, params)
            set_line_width(get_line_width(params))
            @context.move_to(*from_screen(*points.first))
            points[1..-1].each do |x, y|
              @context.line_to(*from_screen(x, y))
            end
            @context.line_to(*from_screen(*points.first)) unless params[:opened]
            apply_cairo_action(filled, params)
          end
        end

        def draw_layout(layout, x, y, color=nil, params={})
          x, y = from_screen(x, y)
          @context.save do
            set_source(color, params)
            set_line_width(get_line_width(params))
            @context.move_to(x, y)
            @context.show_pango_layout(layout)
          end
        end

        def draw_pixbuf(pixbuf, x, y, params={})
          x, y = from_screen(x, y)

          draw_scaled_pixbuf = params[:draw_scaled_pixbuf]
          draw_scaled_pixbuf = @draw_scaled_image if draw_scaled_pixbuf.nil?
          width = (params[:width] || pixbuf.width).to_f
          height = (params[:height] || pixbuf.height).to_f
          if draw_scaled_pixbuf and
              [width, height] != [pixbuf.width, pixbuf.height]
            pixbuf = pixbuf.scale(width, height)
          end
          @context.save do
            @context.translate(x, y)
            unless draw_scaled_pixbuf
              @context.scale(width / pixbuf.width, height / pixbuf.height)
            end
            @context.set_source_pixbuf(pixbuf, 0, 0)
            @context.paint(params[:alpha])
          end

          _draw_reflected_pixbuf(pixbuf, x, y, params[:reflect])
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
          width = (params[:width] || dim.width).to_f
          height = (params[:height] || dim.height).to_f
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
          width = (params[:width] || w).to_f
          height = (params[:height] || h).to_f
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
          set_font_resolution(layout.context)
          @context.update_pango_layout(layout)
          layout
        end

        def create_pango_context
          context = Pango::CairoFontMap.default.create_context
          set_font_resolution(context)
          context
        end

        def set_font_resolution(context)
          context.resolution = @canvas.font_resolution
        end

        private
        def init_engine_color
          @foreground = make_color("black")
          @background = make_color(@background_color)
        end

        def set_source(color, params={})
          pattern = params[:pattern]
          set = false
          if pattern
            set = set_pattern(pattern)
            color ||= Color.parse("#fff0") unless set
          end
          set_color(make_color(color)) unless set
        end

        def set_pattern(info)
          pattern = nil
          case info[:type]
          when :radial
            cx0, cy0, radius0, cx1, cy1, radius1 = info[:base]
            cx0, cy0 = from_screen(cx0, cy0)
            cx1, cy1 = from_screen(cx1, cy1)
            pattern = ::Cairo::RadialPattern.new(cx0, cy0, radius0,
                                                 cx1, cy1, radius1)
            info[:color_stops].each do |offset, r, g, b, a|
              pattern.add_color_stop_rgba(offset, r, g, b, a)
            end
          when :linear
            x, y, w, h = info[:base]
            x, y = from_screen(x, y)
            pattern = ::Cairo::LinearPattern.new(x, y, w, h)
            info[:color_stops].each do |offset, r, g, b, a|
              pattern.add_color_stop_rgba(offset, r, g, b, a)
            end
          end
          @context.set_source(pattern) if pattern
          !pattern.nil?
        end

        def set_color(color)
          @context.set_source_rgba(make_color(color).to_a)
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

        def cairo_action(filled, params={})
          if params[:clip]
            [:clip, params[:clip]]
          elsif filled
            :fill
          else
            :stroke
          end
        end

        def apply_cairo_action(filled, params={})
          action, *other_info = cairo_action(filled, params)
          @context.send(action)
          case action
          when :clip
            block, = other_info
            block.call if block
          end
        end

        def make_matrix(xx, xy, x0, yx, yy, y0)
          ::Cairo::Matrix.new(xx, yx, xy, yy, x0, y0)
        end

        def _draw_reflected_pixbuf(pixbuf, x, y, params)
          return unless params

          params = {} if params == true
          ratio = params[:ratio] || 0.3
          start_alpha = params[:start_alpha] || 0.3

          @context.save do
            width = pixbuf.width
            height = pixbuf.height
            @context.translate(x, y + height * 2)
            reflect_context(:x)
            @context.set_source_pixbuf(pixbuf, 0, 0)
            pattern = ::Cairo::LinearPattern.new(width * 0.5, 0,
                                                 width * 0.5, height)
            pattern.add_color_stop_rgba(0, 0, 0, 0, 0)
            pattern.add_color_stop_rgba(1 - ratio, 0, 0, 0, 0)
            pattern.add_color_stop_rgba(1, 0, 0, 0, start_alpha)
            @context.mask(pattern)
          end
        end
      end
    end
  end
end
