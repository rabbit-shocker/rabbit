require 'rabbit/element/container-element'

module Rabbit
  module Element
    module SlideElement
      include ContainerElement

      def initialize(title_element)
        super(title_element)
      end

      def slide
        self
      end

      def title
        @elements.first.text
      end

      def draw(canvas, simulation=nil)
        if simulation.nil?
          begin
            draw(canvas, true)
            draw(canvas, false)
          rescue StandardError, LoadError
            canvas.logger.warn($!)
          end
        else
          canvas.draw_slide(self, simulation) do
            compile(canvas, 0, 0, canvas.width, canvas.height)
            super(simulation)
          end
        end
      end

      def clear_waiting
        @drawing_index = 0
      end

      def clear_theme
        super
        clear_waiting
        @waited_draw_procs = []
      end

      def last?
        @waited_draw_procs.size <= @drawing_index
      end

      def go_forward
        @drawing_index += 1
      end

      def wait(target, exact=false, &proc)
        @waited_draw_procs << [target, exact, proc]
      end

      def waited_draw_procs(target)
        procs = []
        candidates = @waited_draw_procs[0, @drawing_index]
        candidates = candidates.each_with_index do |(t, exact, proc), i|
          next unless target != t
          if exact
            procs << proc if i == @drawing_index - 1
          else
            procs << proc
          end
        end
        procs
      end
    end
  end
end
