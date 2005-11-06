require "gtk2"

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

    module_function
    def arg_list(arity)
      args = []
      if arity == -1
        args << "*args"
      else
        arity.times do |i|
          args << "arg#{i}"
        end
      end
      args
    end

    def find_path_in_load_path(*name)
      found_path = $LOAD_PATH.find do |path|
        File.readable?(File.join(path, *name))
      end
      if found_path
        File.join(found_path, *name)
      else
        nil
      end
    end

    def unescape_title(title)
      REXML::Text.unnormalize(title).gsub(/\r|\n/, ' ')
    end

    def rotate_pixbuf(pixbuf)
      w = pixbuf.width
      h = pixbuf.height
      n = pixbuf.n_channels
      pixels = pixbuf.pixels
      data = "\0" * pixels.size
      i = 0
      pixels.each_byte do |rgba|
        base, rgba_index = i.divmod(n)
        y, x = base.divmod(w)
        partition_index = (h * (x + 1)) - y - 1
        data[partition_index * n + rgba_index] = rgba
        i += 1
      end
      Gdk::Pixbuf.new(data,
                      pixbuf.colorspace,
                      pixbuf.has_alpha?,
                      pixbuf.bits_per_sample,
                      pixbuf.height,
                      pixbuf.width,
                      h * n)
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

  module HTML
    module_function
    def a_link(start_a, label, label_only)
      result = "["
      result << start_a unless label_only
      result << label
      result << "</a>" unless label_only
      result << "]"
      result
    end
  end

  module DirtyCount
    TOO_DIRTY = 5

    def dirty?
      @dirty_count >= TOO_DIRTY
    end
    
    def dirty
      @dirty_count += TOO_DIRTY / 10.0
    end
    
    def very_dirty
      @dirty_count += TOO_DIRTY
    end
    
    def bit_dirty
      @dirty_count += TOO_DIRTY / 100.0
    end

    def dirty_count_clean
      @dirty_count = 0
    end
    
  end
end
