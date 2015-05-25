# Copyright (C) 2012-2013 Kouhei Sutou <kou@cozmixng.org>
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
require "rabbit/gem-builder"

module Rabbit
  module Task
    class Slide
      include Rake::DSL
      include GetText

      attr_accessor :package_dir, :pdf_dir, :required_rabbit_version
      def initialize
        @logger = Logger.default
        @slide = load_slide_configuration
        @package_dir = "pkg"
        @pdf_dir = "pdf"
        @required_rabbit_version = ">= 2.0.2"
        yield(self) if block_given?
        define
      end

      def spec
        @spec ||= create_spec
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
          spec.files += Dir.glob("theme.rb")
          spec.files += Dir.glob("{COPYING,GPL,README*}")
          spec.files += Dir.glob("rabbit/**/*.*")
          spec.files += Dir.glob("**/*.{svg,png,jpg,jpeg,gif,eps,pdf}")
          spec.files += Dir.glob("*.{rd,rab,hiki,md,pdf}")
          spec.files -= Dir.glob("{pkg,pdf}/**/*.*")
          spec.files += [pdf_path]

          spec.add_runtime_dependency("rabbit", @required_rabbit_version)
        end
      end

      def define
        task :default => :run

        define_run_task
        define_gem_task
        define_pdf_task
        define_publish_task
      end

      def define_run_task
        file options_path do
          format = _("To run rabbit, create '%{options_path}'!")
          raise(format % {:options_path => options_path})
        end

        desc(_("Show slide"))
        task :run => options_path do
          rabbit
        end
      end

      def define_gem_task
        define_gem_create_task
        define_gem_validate_task
      end

      def define_gem_create_task
        desc(_("Create gem: %{gem_path}") % {:gem_path => gem_path})
        task :gem => ["gem:validate", :pdf] do
          mkdir_p(@package_dir)
          GemBuilder.build(spec)
          mv(File.basename(spec.cache_file), gem_path)
        end
      end

      def define_gem_validate_task
        namespace :gem do
          task :validate do
            errors = []
            format = _("Write %{item} in %{where}: %{content}")
            data = {
              :where => Dir.glob("README*")[0],
            }
            [:summary, :description].each do |item|
              content = spec.send(item)
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
      end

      def define_pdf_task
        file pdf_path => [options_path, *(spec.files - [pdf_path])] do
          mkdir_p(@pdf_dir)
          rabbit("--print",
                 "--output-filename", pdf_path)
        end

        desc(_("Generate PDF: %{pdf_path}") % {:pdf_path => pdf_path})
        task :pdf => pdf_path
      end

      def define_publish_task
        desc(_("Publish the slide to all available targets"))
        task :publish

        publish_tasks = []
        namespace :publish do
          if @slide.author.slideshare_user
            define_publish_slideshare_task
            publish_tasks << :slideshare
          end

          if @slide.author.speaker_deck_user
            define_publish_speaker_deck_task
            publish_tasks << :speaker_deck
          end

          if @slide.author.rubygems_user
            define_publish_rubygems_task
            publish_tasks << :rubygems
          end
        end
        task :publish => publish_tasks.collect {|task| "publish:#{task}"}
      end

      def define_publish_rubygems_task
        desc(_("Publish the slide to %s" % "RubyGems.org"))
        task :rubygems => :gem do
          ruby("-S", "gem", "push", gem_path)
        end
      end

      def define_publish_slideshare_task
        slideshare_user = @slide.author.slideshare_user
        desc(_("Publish the slide to %s" % "SlideShare"))
        task :slideshare => [:pdf, "gem:validate"] do
          require "rabbit/slideshare"
          slideshare = SlideShare.new(@logger)
          slideshare.user = slideshare_user
          slideshare.pdf_path = pdf_path
          slideshare.id = @slide.id
          slideshare.title = spec.summary
          slideshare.description = spec.description
          slideshare.tags = @slide.tags if @slide.tags
          url = slideshare.upload
          if url
            @logger.info(_("Uploaded successfully!"))
            @logger.info(_("See %s") % url)
            Gtk.show_uri(url) if Gtk.respond_to?(:show_uri)

            slide_id = url.split(/\//).last
            @slide.slideshare_id = slide_id
            @slide.save(".")
          end
        end
      end

      def define_publish_speaker_deck_task
        desc(_("Publish the slide to %s" % "Speaker Deck"))
        task :speaker_deck => :pdf do
          puts "Not implemented yet."
        end
      end

      def options_path
        ".rabbit"
      end

      def gem_path
        File.join(@package_dir, "#{spec.name}-#{spec.version}.gem")
      end

      def pdf_path
        File.join(@pdf_dir, pdf_base_path)
      end

      def pdf_base_path
        "#{@slide.id}-#{@slide.base_name}.pdf"
      end

      def homepage
        rubygems_user = @slide.author.rubygems_user
        "http://slide.rabbit-shocker.org/authors/#{rubygems_user}/#{@slide.id}/"
      end

      def rabbit(*arguments)
        unless Rabbit::Command::Rabbit.run(*arguments)
          message = "failed to run Rabbit"
          message << ": #{arguments.join(' ')}" unless arguments.empty?
          raise message
        end
      end
    end
  end
end
