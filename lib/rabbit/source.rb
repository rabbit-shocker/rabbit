module Rabbit

  module Source

    normalize = Proc.new do |base_path, path|
      path.sub(/\A#{Regexp.escape(base_path)}\/?/, '').sub(/\.[^.]+$/, '')
    end
    
    $LOAD_PATH.each do |path|
      source_glob = ::File.join(path, 'rabbit', 'source', '*')
      Dir.glob(source_glob) do |source|
        begin
          require normalize[path, source]
        rescue LoadError
        end
      end
    end
    
    def self.types
      constants.collect do |x|
        const_get(x)
      end.find_all do |x|
        x.is_a?(Class)
      end
    end

  end
end
