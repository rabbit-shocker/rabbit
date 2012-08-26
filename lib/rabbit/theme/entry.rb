require 'erb'

require 'rabbit/rabbit'

module Rabbit
  module Theme
    class Entry
      extend ERB::DefMethod
      
      include Enumerable
      include ERB::Util
      include GetText

      PROPERTY_BASE_NAME = "property"

      class << self
        @@template_last_modified_time = nil
        
        def template_path
          path = ["rabbit", "theme", "document.erb"]
          template_path = Utils.find_path_in_load_path(*path)
          if template_path.nil?
            raise CantFindThemeRDTemplate.new(File.join(*path))
          end
          template_path
        end
        
        def load_template(path=nil)
          path ||= template_path
          @@template_last_modified_time = File.mtime(path)
          def_erb_method("to_rd", path)
        end

        def reload_template(path=nil)
          path ||= template_path
          if @@template_last_modified_time < File.mtime(path)
            remove_method("to_rd")
            load_template(path)
          end
        end
      end

      load_template
      
      attr_reader :name, :title, :description
      attr_reader :abstract
      attr_reader :dependencies, :parameters
      attr_accessor :logger

      def initialize(theme_dir, name)
        @logger = nil
        @theme_dir = theme_dir
        @name = name
        @title = @name
        @category = nil
        @abstract = nil
        @description = nil
        @dependencies = []
        @parameters = {}
        parse_property if available?
      end

      def available?
        File.readable?(theme_file)
      end

      def property_editable?
        File.writable?(property_file)
      end

      def <=>(other)
        @name <=> other.name
      end

      def have_file?(target)
        File.exist?(full_path(target))
      end

      def full_path(target)
        File.join(@theme_dir, target)
      end

      def category
        @category || N_("Etc")
      end

      def image_theme?
        false
      end

      def files
        rejected_files = [theme_file, property_file]
        Dir[File.join(@theme_dir, "*")].delete_if do |name|
          rejected_files.include?(name)
        end.sort
      end

      private
      def property_file
         File.join(@theme_dir, "#{PROPERTY_BASE_NAME}.rb")
      end

      def parse_property
        file = property_file
        if File.exist?(file)
          content = File.open(file) {|f| f.read}
          begin
            instance_eval(content, file)
          rescue SyntaxError
            @logger.warn($!) if @logger
          end
        end
      end
    end

    class DirectoryEntry < Entry
      THEME_BASE_NAME = "theme"

      def initialize(theme_dir)
        super(theme_dir, File.basename(theme_dir))
      end

      def theme_file
        @theme_file ||= candidate_theme_files.find do |candidate|
          File.readable?(candidate)
        end
        @theme_file ||= candidate_theme_files.first
      end

      def candidate_theme_files
        [THEME_BASE_NAME, @name].collect do |base_name|
          File.join(@theme_dir, "#{base_name}.rb")
        end
      end
    end

    class ImageDirectoryEntry < DirectoryEntry
      def image_theme?
        true
      end
    end

    class SingleFileEntry < Entry
      def initialize(theme_dir, name)
        super(theme_dir, name)
      end

      def property_editable?
        false
      end

      def theme_file
        File.join(@theme_dir, "#{@name}.rb")
      end

      def files
        []
      end

      def have_file?(target)
        name == target
      end

      private
      def parse_property
      end
    end
  end
end
