require "forwardable"
require "tempfile"
require 'thread'

require "rabbit/image/base"
require "rabbit/image/default"
require "rabbit/image/eps"
begin
  require "rabbit/image/svg"
rescue LoadError
end

module Rabbit
  
  module ImageManipulable

    extend Forwardable

    def_delegators(:@loader, :keep_ratio, :keep_ratio=)
    def_delegators(:@loader, :pixbuf, :width, :height)
    def_delegators(:@loader, :resize)
    alias_method :scale, :resize

    def initialize(filename, *args, &block)
      unless File.exist?(filename)
        raise ImageFileDoesNotExistError.new(filename)
      end
      super(*args, &block)
      @loader = Base.find_loader(filename).new(filename, true)
    end
  end
  
  class ImageLoader

    include ImageManipulable

  end

end
