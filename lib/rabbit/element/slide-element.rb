require 'rabbit/element/container-element'

module Rabbit
  module Element
    module SlideElement
      include ContainerElement

      attr_reader :title
      def initialize(title)
        super()
        @title = title
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
    end
  end
end
