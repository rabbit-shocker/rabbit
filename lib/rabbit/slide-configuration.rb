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
require "rabbit/author-configuration"
require "rabbit/path-manipulatable"

module Rabbit
  class SlideConfiguration
    include GetText
    include PathManipulatable

    attr_accessor :logger
    attr_accessor :id, :base_name, :tags, :presentation_date, :version
    attr_accessor :licenses
    attr_reader :author
    def initialize(logger=nil)
      @logger = logger || Logger.default
      @id = nil
      @base_name = nil
      @tags = []
      @presentation_date = nil
      @version = nil
      @licenses = []
      @author = nil
    end

    def load
      return unless File.exist?(path)
      conf = YAML.load(File.read(path))
      merge!(conf)
    rescue
      format = _("Failed to read slide configuration: %s: %s")
      @logger.error(format % [path, $!.message])
    end

    def save
      create_file(path) do |conf_file|
        conf_file.print(to_yaml)
      end
    rescue
      format = _("Failed to write slide configuration: %s: %s")
      @logger.error(format % [path, $!.message])
    end

    def merge!(conf)
      @id                = conf["id"]
      @base_name         = conf["base_name"]
      @tags              = conf["tags"]
      @presentation_date = conf["presentation_date"]
      @version           = conf["version"]
      @licenses          = conf["licenses"]

      @author = AuthorConfiguration.new
      @author.merge!(conf["author"] || {})
    end

    def to_hash
      config = {
        "id"                => @id,
        "base_name"         => @base_name,
        "tags"              => @tags,
        "presentation_date" => @presentation_date,
        "version"           => @version || default_version,
        "licenses"          => @licenses,
      }
      config["author"] = @author.to_hash if @author
      config
    end

    def to_yaml
      to_hash.to_yaml
    end

    def gem_name
      "#{gem_name_prefix}-#{@author.rubygems_user}-#{@id}"
    end

    def path
      "config.yaml"
    end

    private
    def gem_name_prefix
      "rabbit-slide"
    end

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
