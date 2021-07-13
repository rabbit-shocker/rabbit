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

require "rabbit/gettext"
require "rabbit/author-configuration"
require "rabbit/path-manipulatable"
require "rabbit/yaml-loader"

module Rabbit
  class ThemeConfiguration
    include GetText
    include PathManipulatable

    GEM_NAME_PREFIX = "rabbit-theme"

    attr_accessor :logger
    attr_accessor :id, :tags, :licenses
    attr_writer :version
    attr_accessor :author
    def initialize(logger=nil)
      @logger = logger || Logger.default
      @id = nil
      @tags = []
      @version = nil
      @licenses = []
      @author = nil
    end

    def load
      return unless File.exist?(path)
      conf = YAMLLoader.load(File.read(path))
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

    def merge!(conf)
      @id                = conf["id"]
      @tags              = conf["tags"]
      @version           = conf["version"]
      @licenses          = conf["licenses"]

      @author = AuthorConfiguration.new(@logger)
      @author.merge!(conf["author"] || {})
    end

    def to_hash
      config = {
        "id"                => @id,
        "tags"              => @tags,
        "version"           => version,
        "licenses"          => @licenses,
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
      "#{GEM_NAME_PREFIX}-#{@id}"
    end

    def path
      "config.yaml"
    end

    private
    def default_version
      "1.0.0"
    end
  end
end
