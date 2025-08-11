# Copyright (C) 2014-2024  Sutou Kouhei <kou@cozmixng.org>
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

case ENV["RABBIT_GTK"]
when "4"
  require "gtk4"
else
  require "gtk3"
end

Gtk.init if Gtk.respond_to?(:init)

module Gdk
  class ModifierType
    ALT_MASK = MOD1_MASK unless const_defined?(:ALT_MASK)
    alias_method :alt_mask?, :mod1_mask? unless method_defined?(:alt_mask?)
  end
end

module Gtk
  unless const_defined?(:Native)
    # For GTK 3
    #
    # GTK 4 renames Gdk::Window to Gdk::Surface
    class Widget
      def native
        self
      end

      alias_method :surface, :window
    end
  end

  unless const_defined?(:StackTransitionType)
    StackTransitionType = Stack::TransitionType
  end
end
