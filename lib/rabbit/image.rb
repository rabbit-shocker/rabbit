require "forwardable"
require "tempfile"
require 'thread'

require 'rabbit/utils'

dir = File.join("rabbit", "image")
Rabbit::Utils.require_files_under_directory_in_load_path(dir)

module Rabbit
  
  module ImageManipulable

    extend Forwardable

    def_delegators(:@loader, :keep_ratio, :keep_ratio=)
    def_delegators(:@loader, :pixbuf, :width, :height)
    def_delegators(:@loader, :original_width, :original_height)
    def_delegators(:@loader, :resize)
    alias_method :scale, :resize

    def initialize(filename, *args, &block)
      unless File.exist?(filename)
        raise ImageFileDoesNotExistError.new(filename)
      end
      super(*args, &block)
      @props = {}
      @loader = Base.find_loader(filename).new(filename, true)
    end

    def [](name)
      @props[name]
    end

    def []=(name, value)
      @props[name] = value
    end
  end
  
  class ImageLoader

    include ImageManipulable

  end

end
