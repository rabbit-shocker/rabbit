require "forwardable"

require "rabbit/utils"
require "rabbit/image/base"

module Rabbit
  module ImageManipulable
    class Dia < Base

      unshift_loader(self)

      DIA_COMMANDS = %w(dia)

      extend Forwardable
      include SystemRunner

      class << self
        def match?(filename)
          File.open(filename) do |f|
            if /^<\?xml/ =~ f.gets and
                /http:\/\/www\.lysator\.liu\.se\/~alla\/dia\// =~ f.gets
              true
            else
              false
            end
          end
        rescue ArgumentError
          false
        end
      end

      def_delegators(:@eps_loader, :keep_ratio, :keep_ratio=)
      def_delegators(:@eps_loader, :pixbuf, :internal_pixbuf)
      def_delegators(:@eps_loader, :width, :height)
      def_delegators(:@eps_loader, :original_width, :original_height)
      def_delegators(:@eps_loader, :resize, :ensure_resize)
      def_delegators(:@eps_loader, :update_size)

      def initialize(filename, keep_ratio)
        init_eps_loader(filename, keep_ratio)
        super
      end

      private
      def init_eps_loader(filename, keep_ratio)
        @eps_file = Tempfile.new("rabbit-loader-dia")
        args = ["--export=#{@eps_file.path}"]
        args << "--filter=eps"
        args << filename
        if DIA_COMMANDS.any? {|dia| run(dia, *args)}
          @eps_loader = EPS.new(@eps_file.path, keep_ratio)
        else
          raise DiaCanNotHandleError.new("dia #{args.join(' ')}",
                                         DIA_COMMANDS)
        end
      end

      def load_image
        # do nothing
      end
    end
  end
end
