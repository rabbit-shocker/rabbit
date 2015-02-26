require 'rabbit/gtk'

require 'rabbit/rabbit'

module Rabbit
  class CursorManager
    @@blank_cursor = nil

    attr_accessor :current
    def initialize
      @stocks = {}
      @current = nil
      @blank_cursor = Gdk::Cursor.new(:blank_cursor)
      @pencil_cursor = Gdk::Cursor.new(Gdk::Cursor::PENCIL)
      @hand_cursor = Gdk::Cursor.new(Gdk::Cursor::HAND1)
    end

    def keep(name)
      @stocks[name] ||= []
      @stocks[name].push(@current)
    end

    def restore(drawable, name)
      if name.nil?
        type = @current
      else
        type = @stocks[name].pop
      end
      drawable.cursor = type_to_cursor(type)
    end

    def update(drawable, type)
      drawable.cursor = type_to_cursor(type)
    end

    private
    def type_to_cursor(type)
      if type.nil?
        nil
      else
        name = "@#{type}_cursor"
        unless instance_variable_defined?(name)
          raise UnknownCursorTypeError.new(type)
        end
        instance_variable_get(name)
      end
    end
  end
end

