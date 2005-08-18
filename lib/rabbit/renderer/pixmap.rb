require "rabbit/renderer/pixmap/base"

module Rabbit
  module Renderer
    begin
      require "rabbit/renderer/pixmap/gl"
      Pixmap = PixmapGL
    rescue LoadError
      Pixmap = PixmapBase
    end
  end
end
