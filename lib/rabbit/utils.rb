require "gtk2"

require "rabbit/rabbit"

module Rabbit
  module Utils
    def to_class_name(name)
      name.gsub(/(?:\A|_|\-)([a-z])/) do |x|
        $1.upcase
      end
    end

    def require_files_under_directory_in_load_path(dir)
      normalize = Proc.new do |base_path, path|
        path.sub(/\A#{Regexp.escape(base_path)}\/?/, '').sub(/\.[^.]+$/, '')
      end
    
      $LOAD_PATH.each do |path|
        source_glob = ::File.join(path, dir, '*')
        Dir.glob(source_glob) do |source|
          begin
            require normalize[path, source]
          rescue LoadError
          end
        end
      end
    end

    def collect_classes_under_module(mod)
      mod.constants.collect do |x|
        mod.const_get(x)
      end.find_all do |x|
        x.is_a?(Class)
      end
    end
  end
  
  module SystemRunner
    def run(cmd, *args)
      begin
        system(cmd, *args)
      rescue SystemCallError
        yield($!) if block_given?
        false
      end
    end
  end
  
  module ScreenInfo
    module_function
    def default_screen
      Gdk::Screen.default
    end
    
    def screen_width
      default_screen.width
    end

    def screen_width_mm
      default_screen.width_mm
    end

    def screen_height
      default_screen.height
    end

    def screen_height_mm
      default_screen.height_mm
    end

    def screen_x_resolution
      screen_width / mm_to_inch(screen_width_mm)
    end

    def screen_y_resolution
      screen_height / mm_to_inch(screen_height_mm)
    end

    def screen_depth
      default_screen.root_window.depth
    end
    
    def mm_to_inch(mm)
      mm / 25.4
    end
  end
  
end
