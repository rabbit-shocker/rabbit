# Copyright (C) 2012-2019  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/theme/applier"

class RabbitApplierTest < Test::Unit::TestCase
  def setup
    theme = Object.new
    def theme.slides
      Object.new
    end
    @applier = Rabbit::Theme::Applier.new(theme)
  end

  sub_test_case("#normalize_source") do
    def normalize_source(source)
      @applier.__send__(:normalize_source, source)
    end

    data("x large",
         ["x_large_font_size", "huge_font_size"],
         keep: true)
    data("xx large",
         ["xx_large_font_size", "very_huge_font_size"],
         keep: true)
    data("x large script",
         ["x_large_script_font_size", "huge_script_font_size"],
         keep: true)

    def test_reference
      after, before = data
      assert_equal("@#{after}",
                   normalize_source("@#{before}"))
    end

    def test_reference_in_parentheses
      after, before = data
      assert_equal("(@#{after})",
                   normalize_source("(@#{before})"))
    end

    def test_reference_with_arguments_in_parentheses
      after, before = data
      assert_equal("(xxx, @#{after}, xxx)",
                   normalize_source("(xxx, @#{before}, xxx)"))
    end

    def test_assign_without_space
      after, before = data
      assert_equal("@#{after}=111",
                   normalize_source("@#{before}=111"))
    end

    def test_assign_with_space
      after, before = data
      assert_equal("@#{after} = 111",
                   normalize_source("@#{before} = 111"))
    end
  end
end
