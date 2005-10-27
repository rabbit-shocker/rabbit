#!/usr/bin/env ruby

require 'gtk2'

Gtk.init

width = 100
height = 100

use_area = ARGV.empty?

window = Gtk::Window.new
window.set_default_size(width, height)
window.signal_connect("destroy") do |widget, event|
  Gtk.main_quit
  true
end
window.signal_connect("key_press_event") do |widget, event|
  if event.state.control_mask? and event.keyval == Gdk::Keyval::GDK_q
    widget.destroy
    true
  else
    false
  end
end
window.signal_connect("expose_event") do |widget, event|
  cr = widget.window.create_cairo_context
  rgba = [0xdd, 0xee, 0xff, 0xff].collect{|x| x / 255.0}
  cr.set_source_rgba(rgba)
  cr.rectangle(0, 0, width, height)
  cr.fill
  false
end

layout = Gtk::Layout.new

name = File.join(File.dirname(__FILE__), "ruby-gnome2-logo.png")
loader = Gdk::PixbufLoader.new
File.open(name, "rb") do |f|
  loader.last_write(f.read)
end
pixbuf = loader.pixbuf
image = Gtk::Image.new(pixbuf.scale(width, height))
layout.put(image, 0, 0)

area = Gtk::DrawingArea.new
area.set_size_request(width, height)
area.signal_connect("realize") do |widget, event|
  x, y, w, h = widget.allocation.to_a
  mask = Gdk::Pixmap.new(nil, w, h, 1)

  set_gc = Gdk::GC.new(mask)
  set_gc.function = Gdk::GC::SET
  args = [set_gc, true, 0, 0, w, h]
  mask.draw_rectangle(*args)

  xor_gc = Gdk::GC.new(mask)
  xor_gc.function = Gdk::GC::INVERT
  args = [xor_gc, true, w / 6.0, h / 6.0,
          w * 2.0 / 3.0, h * 2.0 / 3.0,
          0, 360 * 64]
  mask.draw_arc(*args)

  widget.shape_combine_mask(mask, 0, 0)
  false
end
area.signal_connect("expose_event") do |widget, event|
  cr = widget.window.create_cairo_context
  rgba = [0xff, 0xf1, 0xec, 0xcc].collect{|c| c / 255.0}
  cr.set_source_rgba(rgba)
  cr.paint
  false
end

box = Gtk::EventBox.new
box.visible_window = false
box.set_size_request(width, height)
box.signal_connect("expose_event") do |widget, event|
  x, y, w, h = widget.allocation.to_a

  cr = widget.window.create_cairo_context
  cr.translate(x, y)
  cr.scale(w, h)
  
  rgba = [0xff, 0xf1, 0xec, 0xcc].collect{|c| c / 255.0}
  cr.set_source_rgba(rgba)

  cr.rectangle(0, 0, 1, 1)
  cr.arc_negative(0.5, 0.5, 0.4, 2 * Math::PI, 0)
  cr.close_path
  cr.fill
  false
end

if use_area
  layout.put(area, 0, 0)
else
  layout.put(box, 0, 0)
end

window.add(layout)
window.show_all

Gtk.main

