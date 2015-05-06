require 'rabbit/gtk'

require 'rabbit/rabbit'

module Rabbit
  class CursorManager
    @@cursors = nil

    class << self
      def cursors
        @@cursors ||= {
          :blank  => Gdk::Cursor.new(:blank_cursor),
          :pencil => Gdk::Cursor.new(:pencil),
          :hand   => Gdk::Cursor.new(:hand1),
        }
      end
    end

    attr_accessor :current
    def initialize
      @stocks = {}
      @current = nil
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
        cursor = self.class.cursors[type]
        if cursor.nil?
          raise UnknownCursorTypeError.new(type)
        end
        cursor
      end
    end
  end
end

