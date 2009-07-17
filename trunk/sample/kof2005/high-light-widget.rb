#!/usr/bin/env ruby

require 'gtk2'

Gtk.init

class Widget < Gtk::EventBox

  def initialize
    super
    set_visible_window(false)
    add_events(Gdk::Event::ENTER_NOTIFY_MASK)
    add_events(Gdk::Event::LEAVE_NOTIFY_MASK)
    set_enter_notify
    set_leave_notify
  end

  def set_enter_notify
    signal_connect("enter_notify_event") do |widget, event|
      parent.high_light(widget) if parent
      false
    end
  end
  
  def set_leave_notify
    signal_connect("leave_notify_event") do |widget, event|
      parent.high_light_hide if parent
      false
    end
  end
end

class HighLight < Gtk::Misc
  type_register

  def initialize
    super
    set_flags(Gtk::Widget::NO_WINDOW)
    set_export
  end

  def set_export
    signal_connect("expose_event") do |widget, event|
      cr = widget.window.create_cairo_context
      
      x, y, w, h = widget.allocation.to_a
      line_width = cr.line_width / 2
      x += line_width
      y += line_width
      w -= line_width * 2
      h -= line_width * 2
      cr.rectangle(x, y, w, h)
      cr.set_source_rgba([0, 0, 1, 0.6])
      cr.stroke
      false
    end
  end
end

class Layout < Gtk::Layout
  def initialize(high_light)
    super()
    @high_light = high_light
    put(@high_light, 0, 0)
  end

  def high_light(widget)
    x, y, w, h = widget.allocation.to_a
    remove(@high_light)
    put(@high_light, x, y)
    @high_light.set_size_request(w, h)
    @high_light.show
  end

  def high_light_hide
    @high_light.hide
  end
end

window = Gtk::Window.new
window.set_default_size(512, 400)

window.signal_connect("destroy") do
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

hl = HighLight.new
layout = Layout.new(hl)

rect = Widget.new
rect.set_size_request(260, 260)
rect.signal_connect("expose_event") do |widget, event|
  x, y, w, h = widget.allocation.to_a
  cr = widget.window.create_cairo_context
  line_width = cr.line_width / 2
  x += line_width
  y += line_width
  w -= line_width * 2
  h -= line_width * 2
  rgba = [0xff, 0xf1, 0xec, 0xff].collect{|c| c / 255.0}
  cr.set_source_rgba(rgba)
  cr.rectangle(x, y, w, h)
  cr.fill_preserve
  rgba = [0xff, 0x99, 0x99, 0xff].collect{|c| c / 255.0}
  cr.set_source_rgba(rgba)
  cr.stroke
  false
end
layout.put(rect, 80, 30)
  
circ = Widget.new
circ.set_size_request(250, 250)
circ.signal_connect("expose_event") do |widget, event|
  x, y, w, h = widget.allocation.to_a
  cr = widget.window.create_cairo_context
  line_width = cr.line_width / 2
  x += line_width
  y += line_width
  w -= line_width * 2
  h -= line_width * 2
  rgba = [0xcc, 0xff, 0xcc, 0xff].collect{|c| c / 255.0}
  cr.set_source_rgba(rgba)
  r = w / 2
  cr.arc(x + r , y + r, r, 0, 2 * Math::PI)
  cr.fill_preserve
  rgba = [0x00, 0x66, 0x00, 0xff].collect{|c| c / 255.0}
  cr.set_source_rgba(rgba)
  cr.stroke
  false
end
layout.put(circ, 180, 140)

window.add(layout)
window.show_all

hl.hide

Gtk.main
