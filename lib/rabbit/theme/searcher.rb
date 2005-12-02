require 'rabbit/theme/entry'

module Rabbit
  module Theme
    module Searcher
      def initialize(*args, &blocks)
        @theme_stack = []
        @theme_paths = []
        super
      end

      def push_theme(entry)
        @theme_stack.push(entry)
      end

      def pop_theme
        @theme_stack.pop
      end

      def in_theme(entry)
        push_theme(entry)
        yield(entry)
      ensure
        pop_theme
      end

      module_function
      def theme_dir(base_dir)
        File.join(base_dir, 'rabbit', 'theme')
      end
      
      def find_theme(theme_name=name)
        found_entry = nil
        collect_theme do |entry|
          if theme_name == entry.name
            found_entry = entry
            break
          end
        end
        raise LoadError, "can't find theme: #{theme_name}." if found_entry.nil?
        found_entry
      end

      def find_file(target, themes=@theme_paths+@theme_stack)
        found_entry = themes.find do |entry|
          entry.have_file?(target)
        end
        if found_entry.nil?
          names = themes.collect {|entry| entry.name}
          raise LoadError,
                "can't find file in themes #{names.inspect}: #{target}."
        end
        found_entry.full_path(target)
      end

      def collect_theme
        themes = []
        theme_name = {}
        $LOAD_PATH.each do |path|
          base_name = theme_dir(path)
          if File.directory?(base_name)
            Dir.foreach(base_name) do |theme|
              next if /\A..?\z/ =~ theme
              entry = Entry.new(File.join(File.expand_path(base_name), theme))
              if entry.available? and !theme_name.has_key?(theme)
                yield(entry) if block_given?
                themes << entry
                theme_name[theme] = true
              end
            end
          end
        end
        themes.sort
      end
    end
  end
end
