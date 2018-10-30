require 'rabbit/gtk'

require 'rabbit/renderer/color'

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
