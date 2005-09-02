require "rabbit/renderer/pixmap/base"

module Rabbit
  module Renderer
    begin
      raise LoadError, "disable GL support"
      require "rabbit/renderer/pixmap/gl"
      Pixmap = PixmapGL
    rescue LoadError
      Pixmap = PixmapBase
    end
  end
end
