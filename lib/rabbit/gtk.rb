# Copyright (C) 2014-2015  Kouhei Sutou <kou@cozmixng.org>
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

require "English"

if ENV["RABBIT_GTK_VERSION"] == "3"
  require "gtk3"
  Gtk.init if Gtk.respond_to?(:init)
else
  require "gtk2"
end

module Gdk
  class Event
    STOP = true unless const_defined?(:STOP)
    PROPAGATE = false unless const_defined?(:PROPAGATE)
  end

  unless const_defined?(:EventType)
    EventType = Event::Type
  end

  unless const_defined?(:ModifierType)
    ModifierType = Window::ModifierType
  end

  module Keyval
    constants.each do |name|
      if /\AGDK_KEY_/ =~ name.to_s
        const_set("KEY_#{$POSTMATCH}", const_get(name))
      end
    end
  end
end

module Gtk
  class Widget
    unless public_method_defined?(:drag_dest_set)
      def drag_dest_set(flags, targets, actions)
        Gtk::Drag.dest_set(self, flags, targets, actions)
      end
    end
  end
end
