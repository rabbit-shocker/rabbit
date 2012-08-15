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
    def_delegators(:@loader, :x_aspect_ratio, :y_aspect_ratio)
    def_delegators(:@loader, :pixbuf, :width, :height)
    def_delegators(:@loader, :original_width, :original_height)
    def_delegators(:@loader, :resize, :draw)
    def_delegators(:@loader, :[], :[]=)
    alias_method :scale, :resize

    def initialize(filename, props=nil, *args, &block)
      unless File.exist?(filename)
        raise ImageFileDoesNotExistError.new(filename)
      end
      super(*args, &block)
      props = Utils.stringify_hash_key(props) if props
      @loader = Base.find_loader(filename).new(filename, props)
    end
  end

  class ImageLoader
    include ImageManipulable
  end
end
