module Rabbit
  module Parser
    module Ext
      module AAFigure
        module_function
        def make_image_by_aafigure(path, prop, logger)
          image_file = Tempfile.new("rabbit-image-aafigure")
          command = ["aafigure",
                     "--type", "png",
                     "--output", image_file.path,
                     path]
          if SystemRunner.run(*command)
            image_file
          else
            format = _("tried aafigure commands: %s")
            additional_info = format % commands.inspect
            raise AAFigureCanNotHandleError.new(command.join(' '),
                                                additional_info)
          end
        end
      end
    end
  end
end
