module Rabbit
  module Parser
    module Ext
      module BlockDiag
        include GetText

        module_function
        AVAILABLE_FLAG_OPTIONS = ["antialias"]
        # "font" is treated as specially.
        AVAILABLE_VALUE_OPTIONS = []
        def make_image(path, prop, logger)
          image_file = Tempfile.new("rabbit-image-blockdiag")
          command = [
            "blockdiag",
            "-T", "svg",
            "-o", image_file.path,
          ]
          font = find_font(prop)
          command.concat(["-f", font]) if font
          AVAILABLE_FLAG_OPTIONS.each do |name|
            command << "--#{name}" if /\A(?:true|yes)\z/i =~ prop[name].to_s
          end
          AVAILABLE_VALUE_OPTIONS.each do |name|
            command.concat(["--#{name}", prop[name]]) if prop.has_key?(name)
          end
          command << path
          if SystemRunner.run(*command)
            image_file
          else
            format = _("tried blockdiag command: %s")
            additional_info = format % command.inspect
            raise BlockDiagCanNotHandleError.new(command.join(' '),
                                                 additional_info)
          end
        end

        def find_font(prop)
          font = prop["font"]
          return font if font and File.exist?(font)
          fonts = prop["fonts"]
          return nil if fonts.nil?
          fonts = fonts.split(/\s*,\s*/) if fonts.is_a?(String)
          fonts.find do |font|
            File.exist?(font)
          end
        end
      end
    end
  end
end
