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
          return true if File.extname(filename).downcase.end_with?(".dia")
          File.open(filename) do |f|
            first_line = f.gets
            second_line = f.gets
            return false unless second_line
            return false unless first_line.start_with?("<?xml")
            return false unless second_line.start_with?("<dia:diagram")
            true
          end
        end
      end

      def_delegators(:@svg_loader, :keep_ratio, :keep_ratio=)
      def_delegators(:@svg_loader, :pixbuf, :internal_pixbuf)
      def_delegators(:@svg_loader, :width, :height)
      def_delegators(:@svg_loader, :original_width, :original_height)
      def_delegators(:@svg_loader, :resize, :ensure_resize)
      def_delegators(:@svg_loader, :update_size)

      def initialize(filename, keep_ratio)
        init_svg_loader(filename, keep_ratio)
        super
      end

      private
      def init_svg_loader(filename, keep_ratio)
        @svg_file = Tempfile.new(["rabbit-loader-dia", ".svg"])
        args = ["--export=#{@svg_file.path}"]
        args << "--filter=svg"
        args << filename
        if DIA_COMMANDS.any? {|dia| run(dia, *args)}
          @svg_loader = SVG.new(@svg_file.path, keep_ratio)
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
