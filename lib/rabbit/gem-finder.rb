# Copyright (C) 2012-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "gettext"

module Rabbit
  class GemFinder
    include GetText

    def find(name, prefix)
      candidates = [name]
      candidates << "#{prefix}#{name}" unless name.start_with?(prefix)
      normalized_name = name.downcase
      if normalized_name != name
        candidates << normalized_name
        unless normalized_name.start_with?(prefix)
          candidates << "#{prefix}#{normalized_name}"
        end
      end

      candidates.each do |candidate|
        begin
          return Gem::Specification.find_by_name(candidate)
        rescue Gem::LoadError
          require "rubygems/dependency_installer"
          options = {}
          if File.writable?(Gem.dir)
            Rabbit.logger.info(_("Installing gem: %s") % candidate)
          else
            options[:user_install] = true
            format = _("Installing gem in user install mode: %s")
            Rabbit.logger.info(format % candidate)
          end
          installer = Gem::DependencyInstaller.new(options)
          begin
            installer.install(candidate, Gem::Requirement.default)
          rescue Gem::UnsatisfiableDependencyError
          else
            return Gem::Specification.find_by_name(candidate)
          end
        end
      end

      nil
    end
  end
end
