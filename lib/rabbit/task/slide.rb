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
require "rabbit/slide-configuration"
require "rabbit/readme-parser"

module Rabbit
  module Task
    class Slide
      include Rake::DSL
      include GetText

      attr_reader :spec
      attr_accessor :package_dir, :pdf_dir, :required_rabbit_version
      def initialize
        @logger = Logger.default
        @slide = load_slide_configuration
        @spec = create_spec
        @package_dir = "pkg"
        @pdf_dir = "pdf"
        @required_rabbit_version = ">= 2.0.2"
        yield(self) if block_given?
        define
      end

      private
      def load_slide_configuration
        slide_conf = SlideConfiguration.new(@logger)
        slide_conf.load
        slide_conf
      end

      def create_spec
        readme_parser = READMEParser.new(@logger)
        readme_parser.parse

        Gem::Specification.new do |spec|
          spec.name = @slide.gem_name
          spec.version = @slide.version
          spec.homepage = homepage
          spec.authors = [@slide.author.name]
          spec.email = [@slide.author.email]
          spec.summary = readme_parser.title || "TODO"
          spec.description = readme_parser.description || "TODO"
          spec.licenses = @slide.licenses

          slide_conf_path = @slide.path
          spec.files = [".rabbit", slide_conf_path, "Rakefile"]
          spec.files += Dir.glob("{COPYING,GPL,README*}")
          spec.files += Dir.glob("rabbit/**/*.*")
          spec.files += Dir.glob("**/*.{svg,png,jpg,jpeg,gif,eps,pdf}")
          spec.files += Dir.glob("*.{rd,rab,hiki,md,pdf}")
          spec.files -= Dir.glob("{pkg,pdf}/**/*.*")

          spec.add_runtime_dependency("rabbit", @required_rabbit_version)
        end
      end

      def define
        task :default => :run

        options_file = ".rabbit"
        file options_file do
          format = _("To run rabbit, create '%{options_file}'!")
          raise(format % {:options_file => options_file})
        end

        desc(_("Show slide"))
        task :run => options_file do
          rabbit
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
        file pdf_path => [options_file, *@spec.files] do
          mkdir_p(@pdf_dir)
          rabbit("--print",
                 "--output-filename", pdf_path)
        end

        desc(_("Generate PDF: %{pdf_path}") % {:pdf_path => pdf_path})
        task :pdf => pdf_path

        desc(_("Publish the slide to all available targets"))
        task :publish

        publish_tasks = []
        namespace :publish do
          if @slide.author.rubygems_user
            desc(_("Publish the slide to %s" % "RubyGems.org"))
            task :rubygems => :gem do
              ruby("-S", "gem", "push", "--verbose", gem_path)
            end
            publish_tasks << :rubygems
          end

          slideshare_user = @slide.author.slideshare_user
          if slideshare_user
            desc(_("Publish the slide to %s" % "SlideShare"))
            task :slideshare => [:pdf, "gem:validate"] do
              require "rabbit/slideshare"
              slideshare = SlideShare.new(@logger)
              slideshare.user = slideshare_user
              slideshare.pdf_path = pdf_path
              slideshare.title = @spec.summary
              slideshare.description = @spec.description
              slideshare.tags = @tags if @tags
              id = slideshare.upload
              if id
                url = "http://www.slideshare.net/#{slideshare_user}/ss-#{id}"
                @logger.info(_("Uploaded successfully!"))
                @logger.info(_("See %s") % url)
                Gtk.show_uri(url) if Gtk.respond_to?(:show_uri)
              end
            end
            publish_tasks << :slideshare
          end

          if @slide.author.speaker_deck_user
            desc(_("Publish the slide to %s" % "Spearker Deck"))
            task :speaker_deck => :pdf do
              raise "Not implemented yet."
            end
            publish_tasks << :speaker_deck
          end
        end
        task :publish => publish_tasks.collect {|task| "publish:#{task}"}
      end

      def gem_path
        File.join(@package_dir, "#{@spec.name}-#{@spec.version}.gem")
      end

      def pdf_base_path
        "#{@slide.id}.pdf"
      end

      def homepage
        rubygems_user = @slide.author.rubygems_user
        "http://slide.rabbit-shockers.org/#{rubygems_user}/\#{@slide.id}/"
      end

      def rabbit(*arguments)
        Rabbit::Command::Rabbit.run(*arguments)
      end
    end
  end
end
