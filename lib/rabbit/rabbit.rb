module Rabbit

  VERSION = "0.0.3"

  class Error < StandardError
  end
	
  class ImageLoadError < Error
  end

  class ImageFileDoesNotExistError < ImageLoadError
    attr_reader :filename
    def initialize(filename)
      @filename = filename
      super("no such file #{filename}.")
    end
  end

  class ImageLoadWithExternalCommandError < ImageLoadError
    attr_reader :type, :command
    def initialize(type, command, additional_info=nil)
      @type = type
      @command = command
      msg = "#{@type} can't handle, "
      msg << "because command can't be run successfully: #{@command}"
      msg << "\n#{additional_info}" if additional_info
      super(msg)
    end
  end

  class EPSCanNotHandleError < ImageLoadWithExternalCommandError
    def initialize(command, tried_commands)
      additional_info = "tried gs commands: #{tried_commands.inspect}"
      super("EPS", command, additional_info)
    end
  end

  class TeXCanNotHandleError < ImageLoadWithExternalCommandError
    def initialize(command)
      super("TeX", command)
    end
  end

  class UnknownPropertyError < Error
    attr_reader :name
    def initialize(name)
      @name = name
      super("Unknown property: #{name}.")
    end
  end

end
