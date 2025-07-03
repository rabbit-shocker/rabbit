# Copyright (C) 2006-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../gtk"

require_relative "../renderer/color"

module Rabbit
  module Graffiti
    class ConfigDialog
      include GetText

      def initialize(color, line_width)
        @original_color = color
        @original_line_width = line_width
      end

      def run(&block)
        @callback = block
        init_dialog
        if @dialog.run == Gtk::ResponseType::OK
          @callback.call(Renderer::Color.new(@dialog.rgba),
                         nil)
        else
          @callback.call(@original_color, @original_line_width)
        end
        @dialog.destroy
      end

      private
      def init_dialog
        @dialog = Gtk::ColorChooserDialog.new
        @dialog.use_alpha = true
        @dialog.rgba = @original_color.to_gdk_rgba
        add_line_width_control
        @dialog.signal_connect(:color_activated) do |gdk_color|
          @callback.call(Renderer::Color.new(gdk_color), nil)
        end
      end

      def add_line_width_control
        spin = Gtk::SpinButton.new(1, 72, 1)
        spin.value = @original_line_width
        spin.signal_connect("value_changed") do
          @callback.call(nil, spin.value)
        end
        label = Gtk::Label.new(_("Line width:"))
        hbox = Gtk::Box.new(:horizontal)
        hbox.pack_end(spin, :expand => false, :fill => false, :padding => 5)
        hbox.pack_end(label, :expand => false, :fill => false, :padding => 5)
        hbox.show_all
        @dialog.child.pack_end(hbox,
                               :expand => false,
                               :fill => false,
                               :padding => 5)
      end
    end
  end
end
