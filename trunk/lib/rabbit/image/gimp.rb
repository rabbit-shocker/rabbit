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
          File.open(filename) do |f|
            HEADER == f.read(HEADER_SIZE)
          end
        end
      end

      private
      def update_size
        png_file = Tempfile.new("rabbit-loader-gimp-png")
        png_path = png_file.path
        clip_to_image = 1
        merge_type = clip_to_image
        command = <<-EOC
(let ((image (car (gimp-file-load RUN-NONINTERACTIVE
                                  "#{@filename}" "#{@filename}"))))
  (let ((layer (car (gimp-image-merge-visible-layers image #{merge_type}))))
    (file-png-save-defaults RUN-NONINTERACTIVE image layer
                            "#{png_path}" "#{png_path}"))
  (gimp-image-delete image))
EOC
        args = %w(-i)
        args.concat(["-b", command])
        args.concat(["-b", "(gimp-quit TRUE)"])
        if GIMP_COMMANDS.any? {|gimp| run(gimp, *args); File.exist?(png_path)}
          png_file.open
          png_file.binmode
          loader = load_by_pixbuf_loader(png_file.read)
          @pixbuf = loader.pixbuf
        else
          raise GIMPCanNotHandleError.new("gimp #{args.join(' ')}",
                                          GIMP_COMMANDS)
        end
      end
    end
  end
end
