# Copyright (C) 2012  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/image/eps"

class TestImageEPS < Test::Unit::TestCase
  include RabbitTestUtils

  private
  def fixture_path(base_name)
    super("image", "eps", base_name)
  end

  class TestMatch < self
    private
    def match?(base_name)
      Rabbit::ImageManipulable::EPS.match?(fixture_path(base_name))
    end

    def test_have_extension
      assert_true(match?("rabbit.eps"))
    end

    def test_no_extension
      assert_true(match?("rabbit"))
    end
  end
end
