require "rabbit/utils"

dir = File.join("rabbit", "parser")
Rabbit::Utils.require_files_under_directory_in_load_path(dir)

module Rabbit
  module Parser
    module_function
    def parse(canvas, source)
      parser = Base.find_loader(source)
      if parser.nil?
        format = _("unsupported format. (supported: %s)")
        loader_names = Base.loaders.collect {|loader| loader.name}
        message = format % "[#{loader_names.join(', ')}]"
        raise UnsupportedFormatError.new(message)
      end
      parser.new(canvas, source).parse
    end

    def normalize_property_name(name)
      name.gsub(/_/, "-").strip
    end
  end
end
