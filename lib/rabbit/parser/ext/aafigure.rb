module Rabbit
  module Parser
    module Ext
      module AAFigure
        module_function
        def make_image_by_aafigure(path, prop, logger)
          image_file = Tempfile.new("rabbit-image-aafigure")
          command = ["aafigure",
                     "--type", "svg",
                     "--encoding", "utf-8",
                     "--output", image_file.path]
          prop.each do |key, value|
            command.concat(["--#{key}", value])
          end
          command << path
          if SystemRunner.run(*command)
            image_file
          else
            format = _("tried aafigure command: %s")
            additional_info = format % command.inspect
            raise AAFigureCanNotHandleError.new(command.join(' '),
                                                additional_info)
          end
        end
      end
    end
  end
end
