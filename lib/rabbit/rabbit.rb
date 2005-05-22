require "rabbit/gettext"

module Rabbit

  VERSION = "0.0.8"

  TMP_DIR_NAME = ".tmp"

  class Error < StandardError
    include GetText
  end
	
  class ImageLoadError < Error
  end

  class ImageFileDoesNotExistError < ImageLoadError
    attr_reader :filename
    def initialize(filename)
      @filename = filename
      super(_("no such file %s") % filename)
    end
  end

  class ImageLoadWithExternalCommandError < ImageLoadError
    attr_reader :type, :command
    def initialize(type, command, additional_info=nil)
      @type = type
      @command = command
      format =
        _("can't handle %s because the following command can't be run successfully: %s")
      msg = format % [@type, @command]
      msg << "\n#{additional_info}" if additional_info
      super(msg)
    end
  end

  class EPSCanNotHandleError < ImageLoadWithExternalCommandError
    def initialize(command, tried_commands)
      format = _("tried gs commands: %s")
      additional_info = format % tried_commands.inspect
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
      super(_("Unknown property: %s") % name)
    end
  end

  class CantAllocateColorError < Error
    attr_reader :color
    def initialize(color)
      @color = color
      super(_("can't allocate color: %s"), color)
    end
  end

  class SourceUnreadableError < Error
  end
    
  class NotExistError < SourceUnreadableError
    attr_reader :name
    def initialize(name)
      @name = name
      super(_("%s doesn't exist") % @name)
    end
  end
  
  class NotFileError < SourceUnreadableError
    attr_reader :name
    def initialize(name)
      @name = name
      super(_("%s isn't a file") % @name)
    end
  end
  
  class NotReadableError < SourceUnreadableError
    attr_reader :name
    def initialize(name)
      @name = name
      super(_("%s can't be read") % @name)
    end
  end
  
  class ThemeExit < Error
  end

end
