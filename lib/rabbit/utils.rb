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
        yield $! if block_given?
        false
      end
    end
  end
end
