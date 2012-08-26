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

require "rabbit/gettext"

module Rabbit
  class GemFinder
    include GetText

    def initialize(logger=nil)
      @logger = logger || Logger.default
    end

    def find(name, prefix)
      normalized_name = name.downcase
      unless normalized_name.start_with?(prefix)
        normalized_name = "#{prefix}#{normalized_name}"
      end

      retried = false
      spec = nil
      begin
        spec = Gem::Specification.find_by_name(name)
      rescue Gem::LoadError
        begin
          spec = Gem::Specification.find_by_name(normalized_name)
        rescue Gem::LoadError
          unless retried
            retried = true
            require "rubygems/dependency_installer"
            options = {}
            if File.writable?(Gem.dir)
              @logger.info(_("Installing gem: %s") % normalized_name)
            else
              options[:user_install] = true
              format = _("Installing gem in user install mode: %s")
              @logger.info(format % normalized_name)
            end
            installer = Gem::DependencyInstaller.new(options)
            installer.install(normalized_name, Gem::Requirement.default)
            retry
          end
        end
      end
      spec
    end
  end
end
