# Copyright (C) 2005-2012  Kouhei Sutou <kou@cozmixng.org>
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

require 'rbconfig'

module Rabbit
  module Config
    IMAGE_PATH = [
      File.join(File.dirname(__FILE__), "..", "..", "data"),
      RbConfig::CONFIG["datadir"],
    ]
    append_path_from_gem = lambda do
      return unless Object.const_defined?(:Gem)
      rabbit_gem_spec = Gem.loaded_specs["rabbit"]
      return if rabbit_gem_spec.nil?
      unless rabbit_gem_spec.respond_to?(:activated?)
        def rabbit_gem_spec.activated?
          loaded?
        end
      end
      if rabbit_gem_spec.activated?
        IMAGE_PATH.unshift(File.join(rabbit_gem_spec.full_gem_path, "data"))
      end
    end
    append_path_from_gem.call
  end
end
