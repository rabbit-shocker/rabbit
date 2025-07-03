# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
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

require 'tempfile'

module Rabbit
  module Utils
    module_function
    def to_class_name(name)
      name.gsub(/(?:\A|_|\-)([a-z])/) do |x|
        $1.upcase
      end
    end

    def collect_under_module(mod, klass)
      mod.constants.collect do |x|
        mod.const_get(x)
      end.find_all do |x|
        x.is_a?(klass)
      end
    end

    def collect_classes_under_module(mod)
      collect_under_module(mod, Class)
    end

    def collect_modules_under_module(mod)
      collect_under_module(mod, Module)
    end

    def arg_list(arity)
      args = []
      if arity == -1
        args << "*args"
      else
        arity.times do |i|
          args << "arg#{i}"
        end
      end
      args
    end

    def find_path_in_load_path(*name)
      found_path = $LOAD_PATH.find do |path|
        File.readable?(File.join(path, *name))
      end
      if found_path
        File.join(found_path, *name)
      else
        nil
      end
    end

    def unescape_title(title)
      REXML::Text.unnormalize(title).gsub(/\r|\n/, ' ')
    end

    def process_pending_events
      return
      if events_pending_available?
        while Gtk.events_pending?
          Gtk.main_iteration
        end
      end
    end

    def events_pending_available?
      !windows? #or (Gtk::BINDING_VERSION <=> [0, 14, 1]) > 0
    end

    def process_pending_events_proc
      Proc.new do
        process_pending_events
      end
    end

    def init_by_constants_as_default_value(obj)
      klass = obj.class
      klass.constants.each do |name|
        const = klass.const_get(name)
        unless const.kind_of?(Class)
          var_name = name.downcase
          obj.instance_variable_set("@#{var_name}", const.dup)
          klass.module_eval {attr_accessor var_name}
        end
      end
    end

    def windows?
      # Gdk.windowing_win32? # what about this?
      /cygwin|mingw|mswin32|bccwin32/.match(RUBY_PLATFORM) ? true : false
    end

    def support_console_input?
      if windows?
        begin
          File.open("conin$", "w") {}
          true
        rescue SystemCallError
          false
        end
      else
        $stdin.tty? or ENV["TERM"]
      end
    end

    def support_console_output?
      if windows?
        begin
          File.open("conout$", "w") {}
          true
        rescue SystemCallError
          false
        end
      else
        $stdout.tty? or ENV["TERM"]
      end
    end

    def quartz?
      if Gdk.respond_to?(:windowing_quartz?)
        Gdk.windowing_quartz?
      else
        !windows? and !Gdk.windowing_x11?
      end
    end

    def move_to(base, target)
      window = base.window
      screen = window.screen
      num = screen.get_monitor(window)
      monitor = screen.get_monitor_geometry(num)
      window_x, window_y = window.origin
      window_width  = window.width
      window_height = window.height
      target_width, target_height = target.size

      args = [window_x, window_y, window_width, window_height]
      args.concat([target_width, target_height])
      args.concat([screen.width, screen.height])
      x, y = yield(*args)

      target.move(x, y)
    end

    def compute_left_x(base_x)
      [base_x, 0].max
    end

    def compute_right_x(base_x, base_width, target_width, max_x)
      right = base_x + base_width - target_width
      [[right, max_x - target_width].min, 0].max
    end

    def compute_top_y(base_y)
      [base_y, 0].max
    end

    def compute_bottom_y(base_y, base_height, target_height, max_y)
      bottom = base_y + base_height - target_height
      [[bottom, max_y - target_height].min, 0].max
    end

    def move_to_top_left(base, target)
      move_to(base, target) do |bx, by, bw, bh, tw, th, sw, sh|
        [compute_left_x(bx), compute_top_y(by)]
      end
    end

    def move_to_top_right(base, target)
      move_to(base, target) do |bx, by, bw, bh, tw, th, sw, sh|
        [compute_right_x(bx, bw, tw, sw), compute_top_y(by)]
      end
    end

    def move_to_bottom_left(base, target)
      move_to(base, target) do |bx, by, bw, bh, tw, th, sw, sh|
        [compute_left_x(bx), compute_bottom_y(by, bh, th, sh)]
      end
    end

    def move_to_bottom_right(base, target)
      move_to(base, target) do |bx, by, bw, bh, tw, th, sw, sh|
        [compute_right_x(bx, bw, tw, sw),
         compute_bottom_y(by, bh, th, sh)]
      end
    end

    def power_set(elements, &block)
      if block_given?
        0.upto(elements.size) do |i|
          elements.combination(i, &block)
        end
      else
        set = []
        power_set(elements) do |s|
          set << s
        end
        set
      end
    end

    def extract_four_way(params)
      [params[:top], params[:right], params[:bottom], params[:left]]
    end

    def parse_four_way(*values)
      if values.size == 1 and
          (values.first.is_a?(Array) or values.first.is_a?(Hash))
        values = values.first
      end
      if values.is_a?(Hash)
        extract_four_way(values)
      else
        case values.size
        when 1
          left = right = top = bottom = Integer(values.first)
        when 2
          top, left = values.collect {|x| Integer(x)}
          bottom = top
          right = left
        when 3
          top, left, bottom = values.collect {|x| Integer(x)}
          right = left
        when 4
          top, right, bottom, left = values.collect {|x| Integer(x)}
        else
          raise ArgumentError
        end
        [top, right, bottom, left]
      end
    end

    def ensure_time(object)
      return nil if object.nil?
      return object if object.is_a?(Numeric)

      if /\A\s*\z/m =~ object
        nil
      else
        if /\A\s*(\d*\.?\d*)\s*(h|m|s)?\s*\z/i =~ object
          time = $1.to_f
          unit = $2
          if unit
            case unit.downcase
            when "m"
              time *= 60
            when "h"
              time *= 3600
            end
          end
          time.to_i
        else
          nil
        end
      end
    end

    def split_number_to_minute_and_second(number)
      if number >= 0
        sign = " "
      else
        sign = "-"
      end
      [sign, *number.abs.divmod(60)]
    end

    def time(message=nil)
      before = Time.now
      yield
    ensure
      output = Time.now - before
      output = [message, output] if message
      p output
    end

    def stringify_hash_key(hash)
      stringified_hash = {}
      hash.each do |key, value|
        stringified_hash[key.to_s] = value
      end
      stringified_hash
    end

    def syntax_highlighting_debug?
      ENV["RABBIT_SYNTAX_HIGHLIGHTING_DEBUG"] == "yes"
    end
  end

  module SystemRunner
    module_function
    def run(cmd, *args, progress: nil)
      begin
        IO.pipe do |input, output|
          pid = spawn(cmd, *args, out: output)
          output.close
          begin
            loop do
              readables, = IO.select([input], nil, nil, 0.1)
              if readables
                readable = readables[0]
                begin
                  $stdout.print(readable.read_nonblock(4096))
                rescue EOFError
                  break
                else
                  break if readable.eof?
                end
              else
                progress.call if progress
              end
            end
            true
          ensure
            Process.waitpid(pid)
          end
        end
      rescue SystemCallError
        yield($!) if block_given?
        false
      end
    end
  end

  module HTML
    module_function
    def a_link(start_a, label, label_only)
      result = "["
      result << start_a unless label_only
      result << label
      result << "</a>" unless label_only
      result << "]"
      result
    end
  end

  module DirtyCount
    TOO_DIRTY = 5

    def dirty?
      @dirty_count >= TOO_DIRTY
    end

    def dirty(factor=0.1)
      @dirty_count += TOO_DIRTY * factor
      dirtied if dirty?
    end

    def very_dirty
      dirty(1)
    end

    def bit_dirty
      dirty(0.01)
    end

    def dirty_count_clean
      @dirty_count = 0
    end

    private
    def dirtied
      dirty_count_clean
    end

    def check_dirty
      if dirty?
        dirtied
      else
        yield
      end
    end
  end

  module ModuleLoader
    LOADERS = {}

    class << self
      def extend_object(object)
        super
        LOADERS[object] = []
      end
    end

    def loaders
      LOADERS.find do |loader, value|
        self.ancestors.find {|ancestor| ancestor == loader}
      end[1]
    end

    def unshift_loader(loader)
      loaders.unshift(loader)
    end

    def push_loader(loader)
      loaders.push(loader)
    end

    def find_loader(*args)
      loaders.find do |loader|
        loader.match?(*args)
      end
    end
  end

  module TemporaryFile
    module_function
    def create(options={})
      extension = options[:extension]
      content = options[:content]
      source = options[:source]
      if source
        extension ||= source.extension
        content   ||= source.read
      end

      prefix = ["rabbit", options[:prefix]].compact.join("-") + "-"
      if extension
        basename = [prefix, ".#{extension}"]
      else
        basename = prefix
      end
      temp = Tempfile.new(basename)
      if content
        temp.binmode
        temp.print(content)
        temp.close
      end
      yield temp
    end
  end
end
