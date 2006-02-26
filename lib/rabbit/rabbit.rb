begin
  require "rabbit/config"
rescue LoadError
  require "rabbit/default-config"
end

require "rabbit/gettext"

module Rabbit

  VERSION = "0.3.3"

  TMP_DIR_NAME = ".tmp"

  @@gui_init_procs = []

  module_function
  def add_gui_init_proc(proc=Proc.new)
    @@gui_init_procs << proc
  end

  def gui_init
    @@gui_init_procs.each do |proc|
      proc.call
    end
  end

  add_gui_init_proc do
    Gtk.init
  end

  class Error < StandardError
    include GetText
  end

  class ImageLoadError < Error
  end

  class ImageFileDoesNotExistError < ImageLoadError
    attr_reader :filename
    def initialize(filename)
      @filename = filename
      super(_("no such file: %s") % filename)
    end
  end

  class ImageLoadWithExternalCommandError < ImageLoadError
    attr_reader :type, :command
    def initialize(type, command, additional_info=nil)
      @type = type
      @command = command
      format =
        _("can't handle %s because the following command " \
          "can't be run successfully: %s")
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

  class DiaCanNotHandleError < ImageLoadWithExternalCommandError
    def initialize(command, tried_commands)
      format = _("tried dia commands: %s")
      additional_info = format % tried_commands.inspect
      super("Dia", command, additional_info)
    end
  end

  class TgifCanNotHandleError < ImageLoadWithExternalCommandError
    def initialize(command, tried_commands)
      format = _("tried tgif commands: %s")
      additional_info = format % tried_commands.inspect
      super("Tgif", command, additional_info)
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
      super(_("not exist: %s") % @name)
    end
  end

  class NotFileError < SourceUnreadableError
    attr_reader :name
    def initialize(name)
      @name = name
      super(_("not a file: %s") % @name)
    end
  end

  class NotReadableError < SourceUnreadableError
    attr_reader :name
    def initialize(name)
      @name = name
      super(_("can not be read: %s") % @name)
    end
  end

  class ImmutableSourceTypeError < Error
    attr_reader :source_type
    def initialize(source_type)
      @source_type = source_type
      super(_("immutable source type: %s") % @source_type)
    end
  end
  
  class ThemeExit < Error
    def initialize(message=nil)
      @have_message = !message.nil?
      super
    end

    def have_message?
      @have_message
    end
  end

  class NotAvailableInterfaceError < Error
    attr_reader :name
    def initialize(name)
      @name = name
      super(_("not available interface: %s") % @name)
    end
  end

  class CantFindHTMLTemplate < Error
    attr_reader :name
    def initialize(name)
      @name = name
      super(_("can't find HTML template: %s") % @name)
    end
  end

  class CantFindThemeRDTemplate < Error
    attr_reader :name
    def initialize(name)
      @name = name
      super(_("can't find theme RD template: %s") % @name)
    end
  end

  class InvalidMotionError < Error
    attr_reader :motion
    def initialize(motion)
      @motion = motion
      super(_("invalid motion: %s") % @motion)
    end
  end

  class ParseFinish < Error
  end

  class ApplyFinish < Error
  end

  class UnknownCursorTypeError < Error
    attr_reader :type
    def intialize(type)
      @type = type
      super(_("unknown cursor type: %s") % @type)
    end
  end
end
