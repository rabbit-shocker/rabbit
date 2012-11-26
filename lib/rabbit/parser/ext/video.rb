require 'rabbit/element'

module Rabbit
  module Parser
    module Ext
      module Video
        def make_video(canvas, path, prop)
          begin
            Element::Video.new(path, prop)
          rescue Error
            canvas.logger.warn($!.message)
            nil
          end
        end
      end
    end
  end
end
