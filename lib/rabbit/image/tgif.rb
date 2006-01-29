require "forwardable"

require "rabbit/utils"
require "rabbit/image/base"

module Rabbit
  module ImageManipulable
    
    class Tgif < Base

      unshift_loader(self)

      extend Forwardable
      include SystemRunner
      
      class << self
        def match?(filename)
          File.open(filename) do |f|
            if /^%TGIF / =~ f.readline
              true
            else
              false
            end
          end
        end
      end

      def_delegators(:@eps_loader, :keep_ratio, :keep_ratio=)
      def_delegators(:@eps_loader, :pixbuf, :_pixbuf, :_resize, :width, :height)
      def_delegators(:@eps_loader, :original_width, :original_height)
      def_delegators(:@eps_loader, :resize)
      
      def initialize(filename, keep_ratio)
        init_eps_loader(filename, keep_ratio)
        super
      end
      
      private
      def init_eps_loader(filename, keep_ratio)
        tgif = "tgif"
        args = %W(-print -eps -color -quiet #{filename})
        unless run(tgif, *args)
          raise TgifCanNotHandleError.new("#{tgif} #{args.join(' ')}")
        end
        eps_filename = filename.sub(/\.[^.]+\z/, ".eps")
        @eps_loader = EPS.new(eps_filename, keep_ratio)
      end
      
      def load_image
        # do nothing
      end
    end
  end
end
