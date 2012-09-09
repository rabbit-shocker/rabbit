# Copyright (C) 2012  Kouhei Sutou <kou@cozmixng.org>
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

require "time"

require "yaml"

require "rabbit/gettext"
require "rabbit/logger"
require "rabbit/author-configuration"
require "rabbit/path-manipulatable"

module Rabbit
  class SlideConfiguration
    include GetText
    include PathManipulatable

    GEM_NAME_PREFIX = "rabbit-slide"

    attr_accessor :logger
    attr_accessor :id, :base_name, :tags, :presentation_date
    attr_accessor :licenses, :slideshare_id, :speaker_deck_id
    attr_writer :version
    attr_accessor :author
    def initialize(logger=nil)
      @logger = logger || Logger.default
      clear
    end

    def load
      return unless File.exist?(path)
      conf = YAML.load(File.read(path))
      clear
      merge!(conf)
    rescue
      format = _("Failed to read slide configuration: %s: %s")
      @logger.error(format % [path, $!.message])
    end

    def save(base_dir)
      config_path = File.join(base_dir, path)
      create_file(config_path) do |conf_file|
        conf_file.print(to_yaml)
      end
    rescue
      format = _("Failed to write slide configuration: %s: %s")
      @logger.error(format % [config_path, $!.message])
    end

    def clear
      @id                = nil
      @base_name         = nil
      @tags              = []
      @presentation_date = nil
      @version           = nil
      @licenses          = []
      @slideshare_id     = nil
      @speaker_deck_id   = nil
      @author            = nil
    end

    def merge!(conf)
      @id                ||= conf["id"]
      @base_name         ||= conf["base_name"]
      @presentation_date ||= conf["presentation_date"]
      @version           ||= conf["version"]
      @slideshare_id     ||= conf["slideshare_id"]
      @speaker_deck_id   ||= conf["speaker_deck_id"]

      @tags              |=  conf["tags"]
      @licenses          |=  conf["licenses"]

      @author = AuthorConfiguration.new(@logger)
      @author.merge!(conf["author"] || {})
    end

    def to_hash
      config = {
        "id"                => @id,
        "base_name"         => @base_name,
        "tags"              => @tags,
        "presentation_date" => @presentation_date,
        "version"           => version,
        "licenses"          => @licenses,
        "slideshare_id"     => @slideshare_id,
        "speaker_deck_id"   => @speaker_deck_id,
      }
      config["author"] = @author.to_hash if @author
      config
    end

    def to_yaml
      to_hash.to_yaml
    end

    def version
      @version || default_version
    end

    def gem_name
      "#{GEM_NAME_PREFIX}-#{@author.rubygems_user}-#{@id}"
    end

    def path
      "config.yaml"
    end

    private
    def parsed_presentation_date
      return nil if @presentation_date.nil?
      begin
        Time.parse(@presentation_date)
      rescue ArgumentError
        nil
      end
    end

    def default_version
      date = parsed_presentation_date
      if date
        date.strftime("%Y.%m.%d")
      else
        "1.0.0"
      end
    end
  end
end
