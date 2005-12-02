require 'erb'

require 'rabbit/rabbit'

module Rabbit
  module Theme
    class Entry
      extend ERB::DefMethod
      
      include ERB::Util
      include GetText
      include Enumerable
      
      PROPERTY_BASE_NAME = "property"

      path = ["rabbit", "theme", "document.erb"]
      template_path = Utils.find_path_in_load_path(*path)
      raise CantFindThemeRDTemplate.new(File.join(*path)) if template_path.nil?
      def_erb_method("to_rd", template_path)
      
      attr_reader :name, :title, :description
      attr_reader :abstract
      attr_reader :dependencies, :parameters

      def initialize(theme_dir)
        @theme_dir = theme_dir
        @name = File.basename(@theme_dir)
        @title = @name
        @category = nil
        @abstract = nil
        @description = nil
        @dependencies = []
        @parameters = {}
        parse_property if available?
      end

      def available?
        File.exist?(theme_file)
      end
      
      def theme_file
        File.join(@theme_dir, "#{@name}.rb")
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
      
      private
      def property_file
         File.join(@theme_dir, "#{PROPERTY_BASE_NAME}.rb")
      end
      
      def parse_property
        file = property_file
        if File.exist?(file)
          instance_eval(File.open(file) {|f| f.read}, file)
        end
      end
    end
  end
end
