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

module Rabbit
  module Task
    class Theme
      include Rake::DSL
      include GetText

      attr_reader :spec
      attr_accessor :package_dir, :pdf_dir, :pdf_base_path
      attr_accessor :rubygems_user
      def initialize(spec)
        @logger = Logger.default
        @spec = spec
        @package_dir = "pkg"
        @pdf_dir = "pdf"
        @pdf_base_path = nil
        @rubygems_user = nil
        yield(self) if block_given?
        define
      end

      private
      def define
        task :default => :run

        desc(_("Show theme benchmark slide with this theme"))
        task :run do
          rabbit("--theme", "theme", _("rabbit-theme-benchmark-en.gem"))
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

        pdf_path = File.join(@pdf_dir, @pdf_base_path || default_pdf_base_path)
        file pdf_path => [*@spec.files] do
          mkdir_p(@pdf_dir)
          rabbit("--print",
                 "--output-filename", pdf_path,
                 _("rabbit-theme-behcnmark-en.gem"))
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

      def default_pdf_base_path
        theme_id = @spec.name.gsub(/\Arabbit-theme-/, "")
        "#{theme_id}.pdf"
      end

      def rabbit(*arguments)
        Rabbit::Command::Rabbit.run(*arguments)
      end
    end
  end
end
