require 'rabbit/gtk'

require 'rabbit/renderer/color'

module Rabbit
  module Graffiti
    class ConfigDialog
      include GetText

      attr_reader :color, :line_width
      def initialize(color, line_width)
        @original_color = @color = color
        @original_line_width = @line_width = line_width
      end

      def run(&block)
        @callback = block
        init_dialog
        if @dialog.run != Gtk::Dialog::RESPONSE_OK
          @callback.call(@original_color, @original_line_width)
        end
        @dialog.destroy
      end

      private
      def init_dialog
        @dialog = Gtk::ColorSelectionDialog.new
        colorsel = @dialog.colorsel
        colorsel.has_opacity_control = true
        colorsel.has_palette = true
        r, g, b, a = @original_color.to_gdk_rgba
        colorsel.set_current_color(Gdk::Color.new(r, g, b))
        colorsel.set_current_alpha(a)
        add_line_width_control
        colorsel.signal_connect("color_changed") do
          color = Renderer::Color.new_from_gdk_color(colorsel.current_color)
          color.have_alpha = true
          alpha = colorsel.current_alpha / Renderer::Color::GDK_COLOR_NORMALIZE
          color.alpha = alpha
          @callback.call(color, nil)
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
        @dialog.vbox.pack_end(hbox, :expand => false, :fill => false, :padding => 5)
      end
    end
  end
end
