require 'rabbit/theme/entry'

module Rabbit
  module Theme
    module Searcher
      def initialize(*args, &blocks)
        @theme_stack = []
        @image_entries = []
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

      def add_image_path(name)
        @image_entries << find_theme(name, true)
      end

      # for backward compatibility
      alias add_theme_path add_image_path
      
      module_function
      def theme_dir(base_dir)
        File.join(base_dir, 'rabbit', 'theme')
      end
      
      def image_dir(base_dir)
        File.join(base_dir, 'rabbit', 'image')
      end

      def find_theme(theme_name=name, only_image=false)
        if only_image
          collector = "collect_image_theme"
        else
          collector = "collect_all_theme"
        end

        unless only_image
          [base_directory, "."].each do |directory|
            entry = Entry.new(@logger, directory, theme_name)
            return entry if entry.available?
          end
        end

        found_entry = nil
        __send__(collector) do |entry|
          if theme_name == entry.name
            found_entry = entry
            break
          end
        end

        if found_entry.nil?
          if only_image
            entry = ImageGemEntry.new(@logger, theme_name)
          else
            entry = GemEntry.new(@logger, theme_name)
          end
          return entry if entry.available?
          raise LoadError, "can't find theme: #{theme_name}."
        end

        found_entry
      end

      def find_file(target, themes=nil)
        themes ||= @theme_stack + @image_entries
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

      def collect_all_theme(&block)
        theme_names = {}
        themes = []
        callback = Proc.new do |entry|
          unless theme_names.has_key?(entry.name)
            theme_names[entry.name] = true
            themes << entry
            block.call(entry) if block
          end
        end
        collect_image_theme(&callback)
        collect_theme(&callback)
        themes.sort
      end

      def collect_theme(&block)
        _collect_theme(theme_load_path, Version1StyleEntry, &block)
      end

      def collect_image_theme(&block)
        _collect_theme(image_load_path, ImageVersion1StyleEntry, "image_dir",
                       &block)
      end

      def theme_load_path
        $LOAD_PATH
      end

      def image_load_path
        Config::IMAGE_PATH + $LOAD_PATH
      end

      def _collect_theme(path, entry_class, converter=nil, &block)
        converter ||= "theme_dir"
        themes = []
        theme_names = {}
        path.each do |dir|
          base_name = __send__(converter, dir)
          if File.directory?(base_name)
            Dir.foreach(base_name) do |theme|
              next if /\A..?\z/ =~ theme
              next if theme_names.has_key?(theme)
              theme_dir = File.join(File.expand_path(base_name), theme)
              entry = entry_class.new(@logger, theme_dir)
              if entry.available?
                block.call(entry) if block
                themes << entry
                theme_names[theme] = true
              end
            end
          end
        end
        themes.sort
      end
    end
  end
end
