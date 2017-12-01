# Copyright (C) 2017  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit-test-utils"

require "rabbit/canvas"

class RabbitCanvasTest < Test::Unit::TestCase
  setup do
    @logger = Rabbit::Logger::STDERR.new
    @canvas = Rabbit::Canvas.new(@logger, Rabbit::Renderer::Offscreen)
  end

  def create_slide(title)
    title_element = Rabbit::Element::Text.new(title)
    slide = Rabbit::Element::Slide.new(title_element)
    slide
  end

  sub_test_case("#have_previous?") do
    test("no slides") do
      assert_false(@canvas.have_previous?)
    end

    test("first slide") do
      @canvas.slides << create_slide("Title")
      @canvas.slides << create_slide("Page1")
      @canvas.move_to_first
      assert_false(@canvas.have_previous?)
    end

    test("last slide") do
      @canvas.slides << create_slide("Title")
      @canvas.slides << create_slide("Page1")
      @canvas.move_to_last
      assert_true(@canvas.have_previous?)
    end
  end

  sub_test_case("#have_next?") do
    test("no slides") do
      assert_false(@canvas.have_next?)
    end

    test("first slide") do
      @canvas.slides << create_slide("Title")
      @canvas.slides << create_slide("Page1")
      @canvas.move_to_first
      assert_true(@canvas.have_next?)
    end

    test("last slide") do
      @canvas.slides << create_slide("Title")
      @canvas.slides << create_slide("Page1")
      @canvas.move_to_last
      assert_false(@canvas.have_next?)
    end
  end
end
