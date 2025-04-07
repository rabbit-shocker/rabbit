require "rabbit/utils"
require "rabbit/image/base"

module Rabbit
  module ImageManipulable
    class GIMP < Base

      unshift_loader(self)

      GIMP_COMMANDS = %w(gimp)
      HEADER = "gimp xcf file"
      HEADER_SIZE = HEADER.size

      include SystemRunner

      class << self
        def match?(filename)
          File.open(filename, "rb") do |f|
            HEADER == f.read(HEADER_SIZE)
          end
        end
      end

      private
      def update_size
        png_file = Tempfile.new(["rabbit-loader-gimp-png", ".png"])
        png_path = png_file.path
        clip_to_image = 1
        merge_type = clip_to_image
        command = <<-COMMAND
(let ((image (car (gimp-file-load RUN-NONINTERACTIVE
                                  "#{@filename}" "#{@filename}"))))
  (gimp-file-save RUN-NONINTERACTIVE image "#{png_path}")
  (gimp-image-delete image))
        COMMAND
        args = [
          "--no-interface",
          "--batch-interpreter", "plug-in-script-fu-eval",
          "--batch", command,
          "--batch", "(gimp-quit TRUE)",
        ]
        if GIMP_COMMANDS.any? {|gimp| run(gimp, *args); File.exist?(png_path)}
          png_file.open
          png_file.binmode
          load_data(png_file.read)
        else
          raise GIMPCanNotHandleError.new("gimp #{args.join(' ')}",
                                          GIMP_COMMANDS)
        end
      end
    end
  end
end
