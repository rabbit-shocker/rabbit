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

require "yaml"

require "rabbit/gettext"
require "rabbit/path-manipulatable"

module Rabbit
  class AuthorConfiguration
    include GetText
    include PathManipulatable

    attr_accessor :logger
    attr_accessor :name, :email, :markup_language
    attr_accessor :rubygems_user, :slideshare_user, :speaker_deck_user
    def initialize(logger=nil)
      @logger = logger || Logger.default
      @conf_path = File.expand_path("~/.rabbit/author.yaml")
      @markup_language = nil
      @name = nil
      @email = nil
      @rubygems_user = nil
      @slideshare_user = nil
      @speaker_deck_user = nil
    end

    def load
      return unless File.exist?(@conf_path)
      conf = YAML.load(File.read(@conf_path))
      @markup_language   = conf["markup_language"]
      @name              = conf["name"]
      @email             = conf["email"]
      @rubygems_user     = conf["rubygems_user"]
      @slideshare_user   = conf["slideshare_user"]
      @speaker_deck_user = conf["speaker_deck_user"]
    rescue
      format = _("Failed to read author configuration: %s: %s")
      @logger.error(format % [@conf_path, $!.message])
    end

    def save
      conf = {
        "markup_language"   => @markup_language,
        "name"              => @name,
        "email"             => @email,
        "rubygems_user"     => @rubygems_user,
        "slideshare_user"   => @slideshare_user,
        "speaker_deck_user" => @speaker_deck_user,
      }
      create_directory(File.dirname(@conf_path))
      create_file(@conf_path) do |conf_file|
        conf_file.print(conf.to_yaml)
      end
    rescue
      format = _("Failed to write author configuration: %s: %s")
      @logger.error(format % [@conf_path, $!.message])
    end
  end
end
