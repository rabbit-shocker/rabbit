require 'rabbit/element/container-element'
require 'rabbit/element/text-renderer'

module Rabbit
  module Element
    module TextContainerElement
      include ContainerElement
      include TextRenderer

      attr_reader :prop

      alias prop_set __prop_set__
      alias prop_get __prop_get__
      alias prop_delete __prop_delete__

      def draw_elements(canvas, x, y, w, h, simulation)
        unless simulation
          # TODO: This is too workaround. :<
          draw_sub_elements(canvas, x, y, w, h) do
            draw_layout(canvas, x, y)
          end
        end
        [x, y + @height, w, h - @height]
      end

      def to_html(generator)
        html = @elements.collect do |elem|
          elem.to_html(generator)
        end.join("")
        markup_as_html(html)
      end

      def markuped_text
        mt = @elements.collect do |elem|
          elem.markuped_text
        end.join("")
        markup(mt)
      end

      def text
        @elements.collect do |elem|
          elem.text
        end.join("")
      end

      def to_rd
        text
      end

      def do_horizontal_centering?
        super and not width.nil?
      end

      def compile(canvas, x, y, w, h)
        compile_element(canvas, x, y, w, h)
        text_compile(canvas, @x, @y, @w, @h)
      end

      def dirty?
        super or text_dirty?
      end

      def empty?
        /\A\s*\z/ =~ text
      end

      def clear_theme
        container_clear_theme
        super
      end

      def have_numerical_font_size?
        super or @elements.any? {|elem| elem.have_numerical_font_size?}
      end

      def inline_element?
        true
      end

      protected
      def initial_font_size_for_compute_font_size
        [
         super,
         @elements.collect do |elem|
           elem.initial_font_size_for_compute_font_size
         end
        ]
      end

      def compute_next_font_size(previous_size, scale)
        i = -1
        my_font_size, elements_font_size = previous_size
        [
         super(my_font_size, scale),
         elements_font_size.collect do |size|
           i += 1
           @elements[i].compute_next_font_size(size, scale)
         end
        ]
      end

      def set_computed_font_size(new_size)
        my_font_size, elements_font_size = new_size
        elements_font_size.each_with_index do |size, i|
          dirty! if @elements[i].set_computed_font_size(size)
        end
        super(my_font_size)
      end

      private
      # TODO: This is too workaround. :<
      def draw_sub_elements(canvas, x, y, w, h)
        draw_sub_elements_recursive(canvas, x, y, w, h, 0) do
          yield
        end
      end

      # TODO: This is too workaround. :<
      def draw_sub_elements_recursive(canvas, x, y, w, h, i)
        if i == @elements.size
          yield
        else
          draw_sub_elements_recursive(canvas, x, y, w, h, i + 1) do
            element = @elements[i]
            if element.respond_to?(:draw_sub_elements)
              element.draw_sub_elements(canvas, x, y, w, h) do
                yield
              end
            else
              yield
            end
          end
        end
      end
    end
  end
end
