module Rabbit
  module Parser
    module Ext
      module AAFigure
        module_function
        AVAILABLE_OPTIONS = ["linewidth", "foreground", "fill", "background",
                             "option"]
        def make_image_by_aafigure(path, prop, logger)
          image_file = Tempfile.new("rabbit-image-aafigure")
          command = ["aafigure",
                     "--type", "svg",
                     "--encoding", "utf-8",
                     "--output", image_file.path]
          aafigure_options = []
          AVAILABLE_OPTIONS.each do |name|
            command.concat(["--#{name}", prop[name]]) if prop.has_key?(name)
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
