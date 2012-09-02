# Copyright (C) 2012 Kouhei Sutou <kou@cozmixng.org>
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

require "rake"

require "rabbit/gettext"
require "rabbit/logger"
require "rabbit/command/rabbit"
require "rabbit/theme-configuration"
require "rabbit/readme-parser"

module Rabbit
  module Task
    class Theme
      include Rake::DSL
      include GetText

      attr_reader :spec
      attr_accessor :package_dir, :pdf_dir, :required_rabbit_version
      def initialize
        @logger = Logger.default
        @theme = load_theme_configuration
        @spec = create_spec
        @package_dir = "pkg"
        @pdf_dir = "pdf"
        @required_rabbit_version = ">= 2.0.2"
        yield(self) if block_given?
        define
      end

      private
      def load_theme_configuration
        theme_conf = ThemeConfiguration.new(@logger)
        theme_conf.load
        theme_conf
      end

      def create_spec
        readme_parser = READMEParser.new(@logger)
        readme_parser.parse

        Gem::Specification.new do |spec|
          spec.name = @theme.gem_name
          spec.version = @theme.version
          spec.homepage = homepage
          spec.authors = [@theme.author.name]
          spec.email = [@theme.author.email]
          spec.summary = readme_parser.title || "TODO"
          spec.description = readme_parser.description || "TODO"
          spec.licenses = @theme.licenses

          theme_conf_path = @theme.path
          spec.files = [theme_conf_path, "Rakefile"]
          spec.files += Dir.glob("{theme.rb,COPYING,GPL,README*}")
          spec.files += Dir.glob("data/**/*.{svg,png,jpg,jpeg,gif,eps,pdf}")
          spec.files += Dir.glob("locale/**/*.mo")
          spec.files += Dir.glob("po/*/*.po")

          spec.add_runtime_dependency("rabbit", @required_rabbit_version)
        end
      end

      def define
        task :default => :run

        desc(_("Show theme benchmark slide with this theme"))
        task :run do
          rabbit("--theme", ".", _("rabbit-theme-benchmark-en.gem"))
        end

        desc(_("Create gem: %{gem_path}") % {:gem_path => gem_path})
        task :gem => "gem:validate" do
          mkdir_p(@package_dir)
          Gem::Builder.new(@spec).build
          mv(File.basename(@spec.cache_file), gem_path)
        end

        namespace :gem do
          task :validate do
            errors = []
            format = _("Write %{item} in %{where}: %{content}")
            data = {
              :where => Dir.glob("README*")[0],
            }
            [:summary, :description].each do |item|
              content = @spec.send(item)
              if /TODO|FIXME/ =~ content
                data[:item] = item
                data[:content] = content
                errors << (format % data)
              end
            end
            unless errors.empty?
              raise errors.join("\n")
            end
          end
        end

        pdf_path = File.join(@pdf_dir, pdf_base_path)
        file pdf_path => [*@spec.files] do
          mkdir_p(@pdf_dir)
          rabbit("--theme", ".",
                 "--print",
                 "--output-filename", pdf_path,
                 _("rabbit-theme-benchmark-en.gem"))
        end

        desc(_("Generate PDF: %{pdf_path}") % {:pdf_path => pdf_path})
        task :pdf => pdf_path

        desc(_("Publish the theme to all available targets"))
        task :publish

        publish_tasks = []
        namespace :publish do
          if @rubygems_user
            desc(_("Publish the theme to %s") % "RubyGems.org")
            task :rubygems => :gem do
              ruby("-S", "gem", "push", "--verbose", gem_path)
            end
            publish_tasks << :rubygems
          end
        end
        task :publish => publish_tasks.collect {|task| "publish:#{task}"}
      end

      def gem_path
        File.join(@package_dir, "#{@spec.name}-#{@spec.version}.gem")
      end

      def pdf_base_path
        "#{@theme.id}.pdf"
      end

      def homepage
        "http://theme.rabbit-shockers.org/#{@theme.id}/"
      end

      def rabbit(*arguments)
        Rabbit::Command::Rabbit.run(*arguments)
      end
    end
  end
end
