# Copyright (C) 2012-2021  Sutou Kouhei <kou@cozmixng.org>
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

require "date"
require "time"

require "rabbit/gettext"
require "rabbit/logger"
require "rabbit/author-configuration"
require "rabbit/path-manipulatable"
require "rabbit/yaml-loader"

module Rabbit
  class SlideConfiguration
    include GetText
    include PathManipulatable

    GEM_NAME_PREFIX = "rabbit-slide"

    attr_accessor :logger
    attr_accessor :id
    attr_accessor :base_name
    attr_accessor :tags
    attr_reader :presentation_date
    attr_reader :presentation_start_time
    attr_reader :presentation_end_time
    attr_accessor :licenses
    attr_accessor :slideshare_id
    attr_accessor :speaker_deck_id
    attr_accessor :vimeo_id
    attr_accessor :youtube_id
    attr_writer :version
    attr_accessor :author
    attr_accessor :width
    attr_accessor :height
    attr_accessor :source_code_uri
    def initialize(logger=nil)
      @logger = logger || Logger.default
      clear
    end

    def presentation_date=(value)
      @presentation_date = ensure_date(value)
    end

    def presentation_start_time=(value)
      @presentation_start_time = ensure_time(value)
    end

    def presentation_end_time=(value)
      @presentation_end_time = ensure_time(value)
    end

    def load
      return unless File.exist?(path)
      conf = YAMLLoader.load(File.read(path))
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
      @presentation_start_time = nil
      @presentation_end_time = nil
      @version           = nil
      @licenses          = []
      @slideshare_id     = nil
      @speaker_deck_id   = nil
      @vimeo_id          = nil
      @youtube_id        = nil
      @author            = nil
      @width             = 800
      @height            = 600
      @source_code_uri   = nil
    end


    def merge!(conf)
      @id                = conf["id"]                || @id
      @base_name         = conf["base_name"]         || @base_name
      self.presentation_date =
        conf["presentation_date"] || @presentation_date
      self.presentation_start_time =
        conf["presentation_start_time"] || @presentation_start_time
      self.presentation_end_time =
        conf["presentation_end_time"] || @presentation_end_time
      @version           = conf["version"]           || @version
      @slideshare_id     = conf["slideshare_id"]     || @slideshare_id
      @speaker_deck_id   = conf["speaker_deck_id"]   || @speaker_deck_id
      @vimeo_id          = conf["vimeo_id"]          || @vimeo_id
      @youtube_id        = conf["youtube_id"]        || @youtube_id

      @tags              |=  (conf["tags"] || [])
      @licenses          |=  (conf["licenses"] || [])

      @author ||= AuthorConfiguration.new(@logger)
      @author.merge!(conf["author"] || {})

      @width             = conf["width"]             || @width
      @height            = conf["height"]            || @height
      @source_code_uri   = conf["source_code_uri"]   || @source_code_uri
    end

    def to_hash
      config = {
        "id"                => @id,
        "base_name"         => @base_name,
        "tags"              => @tags,
        "presentation_date" => @presentation_date,
        "presentation_start_time" => @presentation_start_time,
        "presentation_end_time" => @presentation_end_time,
        "version"           => version,
        "licenses"          => @licenses,
        "slideshare_id"     => @slideshare_id,
        "speaker_deck_id"   => @speaker_deck_id,
        "vimeo_id"          => @vimeo_id,
        "youtube_id"        => @youtube_id,
        "width"             => @width,
        "height"            => @height,
        "source_code_uri"   => @source_code_uri,
      }
      config["author"] = @author.to_hash if @author
      config
    end

    def to_yaml
      hash = to_hash
      hash.each do |key, value|
        case value
        when Date
          hash[key] = value.strftime("%Y-%m-%d")
        when Time
          hash[key] = value.iso8601
        end
      end
      hash.to_yaml
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
    def ensure_date(value)
      if value.is_a?(String)
        Date.parse(value)
      else
        value
      end
    end

    def ensure_time(value)
      if value.is_a?(String)
        Time.parse(value)
      else
        value
      end
    end

    def default_version
      date = presentation_date
      if date
        "#{date.year}.#{date.month}.#{date.day}.0"
      else
        "1.0.0"
      end
    end
  end
end
