# Copyright (C) 2005-2025  Sutou Kouhei <kou@cozmixng.org>
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

must_set_variables = %w(foreground background color light_color bright_color
                        graffiti_color)

@color_circle_open_quote_image ||= nil
@color_circle_close_quote_image ||= nil

not_set_variables = []
must_set_variables.each do |name|
  variable_name = "@color_circle_#{name}"
  if instance_variable_get(variable_name).nil?
    not_set_variables << variable_name
  end
end

unless not_set_variables.empty?
  format = _("required variables aren't set: %s")
  Rabbit.logger.error(format % not_set_variables.inspect)
  theme_exit
end

set_graffiti_color(@color_circle_graffiti_color)

set_progress_foreground(@color_circle_color)
set_progress_background(@color_circle_bright_color)

include_theme("default-icon")

include_theme("newline-in-slides")
include_theme("tag")

include_theme("image")
include_theme("table")

include_theme("color-circle-title-slide")
include_theme("color-circle-title-text")
include_theme("color-circle-slide")
include_theme("color-circle-text")
include_theme("color-circle-item-mark")
include_theme("color-circle-preformatted")
include_theme("color-circle-block-quote")
include_theme("color-circle-foot-text")
include_theme("color-circle-description")
include_theme("color-circle-method-list")
