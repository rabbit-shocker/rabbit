# Copyright (C) 2021-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "csv"

module Rabbit
  module Parser
    module Ext
      module Charty
        include GetText

        module_function
        def make_image(path, prop)
          require "charty"
          backend = prop["backend"]
          ::Charty::Backends.use(backend) if backend
          font_family = prop["font-family"]
          if font_family and backend == "pyplot"
            default_font_family = ::Matplotlib.rcParams["font.sans-serif"]
            ::Matplotlib.rcParams["font.sans-serif"] =
              [font_family, *default_font_family]
          end
          data = CSV.read(path, headers: true, converters: :all)
          type = prop["type"]
          case type
          when "bar"
            plotter = ::Charty.bar_plot(data: data,
                                        x: prop["x"],
                                        y: prop["y"],
                                        color: prop["color"])
          when "line"
            plotter = ::Charty.line_plot(data: data,
                                         x: prop["x"],
                                         y: prop["y"],
                                         color: prop["color"])
          when "scatter"
            plotter = ::Charty.scatter_plot(data: data,
                                            x: prop["x"],
                                            y: prop["y"],
                                            color: prop["color"])
          else
            raise ArgumentError, "charty: unsupported type: #{type.inspect}"
          end
          image_file = Tempfile.new(["rabbit-image-charty", ".svg"])
          plotter.save(image_file.path)
          if font_family and backend == "pyplot"
            ::Matplotlib.rcParams["font.sans-serif"] = default_font_family
          end
          image_file
        end
      end
    end
  end
end
