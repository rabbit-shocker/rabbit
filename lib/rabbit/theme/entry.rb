# Copyright (C) 2005-2012  Kouhei Sutou <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require 'erb'

require 'rabbit/rabbit'
require "rabbit/gem-finder"

module Rabbit
  module Theme
    class Entry
      extend ERB::DefMethod
      
      include Enumerable
      include ERB::Util
      include GetText

      THEME_BASE_NAME = "theme"
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

      def initialize(logger, theme_dir, name)
        @logger = logger
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
        if have_data_dir?
          File.join(data_dir, target)
        else
          # backward compatibility
          File.join(@theme_dir, target)
        end
      end

      def category
        @category || N_("Etc")
      end

      def image_theme?
        have_data_dir?
      end

      def data_dir
        File.join(@theme_dir, "data")
      end

      def files
        if have_data_dir?
          Dir.glob(File.join(data_dir, "*")).sort
        else
          # backward compatibility
          rejected_files = [theme_file, property_file]
          Dir[File.join(@theme_dir, "*")].delete_if do |name|
            rejected_files.include?(name)
          end.sort
        end
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

      def have_data_dir?
        File.directory?(data_dir)
      end
    end

    class FileEntry < Entry
      def theme_file
        File.join(@theme_dir, "#{@name}.rb")
      end
    end

    class ImageFileEntry < FileEntry
      def image_theme?
        true
      end
    end

    class DirectoryEntry < Entry
      def theme_file
        File.join(@theme_dir, "#{THEME_BASE_NAME}.rb")
      end
    end

    class ImageDirectoryEntry < DirectoryEntry
      def image_theme?
        true
      end
    end

    class GemEntry < Entry
      def initialize(logger, name)
        @spec = nil
        if valid_gem_name?(name)
          finder = GemFinder.new(logger)
          begin
            @spec = finder.find(name, "rabbit-theme-")
          rescue Gem::GemNotFoundException
          end
        end
        theme_dir = nil
        theme_dir = @spec.gem_dir if @spec
        super(logger, theme_dir, name)
      end

      def available?
        @theme_dir and super
      end

      def theme_file
        File.join(@theme_dir, "#{THEME_BASE_NAME}.rb")
      end

      def full_path(target)
        File.join(data_dir, target)
      end

      def files
        Dir.glob(File.join(data_dir, "*")).sort
      end

      private
      def valid_gem_name?(name)
        /\A[a-z\d_\-]\z/i =~ name
      end
    end

    class ImageGemEntry < GemEntry
      def available?
        @theme_dir and File.directory?(data_dir)
      end

      def image_theme?
        true
      end
    end
  end
end
