require "forwardable"

require "rabbit/utils"
require "rabbit/image/base"

module Rabbit
  module ImageManipulable
    class Tgif < Base

      unshift_loader(self)

      TGIF_COMMANDS = %(tgif)

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
        obj_file = Tempfile.new("rabbit-loader-tgif-obj")
        obj_path = "#{obj_file.path}.obj"
        eps_path = "#{obj_file.path}.eps"
        FileUtils.cp(filename, obj_path)
        args = %W(-print -eps -color -quiet #{obj_path})
        begin
          if TGIF_COMMANDS.any? {|tgif| run(tgif, *args); File.exist?(eps_path)}
            @eps_file = Tempfile.new("rabbit-loader-tgif")
            FileUtils.mv(eps_path, @eps_file.path)
            @eps_loader = EPS.new(@eps_file.path, keep_ratio)
          else
            raise TgifCanNotHandleError.new("tgif #{args.join(' ')}",
                                            TGIF_COMMANDS)
          end
        ensure
          FileUtils.rm_f(obj_path)
          FileUtils.rm_f(eps_path)
        end
      end

      def load_image
        # do nothing
      end
    end
  end
end
