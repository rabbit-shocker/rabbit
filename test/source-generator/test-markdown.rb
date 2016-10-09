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

require "rabbit/source-generator/markdown"

class TestSourceGeneratorMarkdown < Test::Unit::TestCase
  def setup
    @generator = Rabbit::SourceGenerator::Markdown.new
  end

  def test_heading1
    assert_equal("# Hello",
                 @generator.heading(1, "Hello"))
  end

  def test_heading2
    assert_equal("## Hello",
                 @generator.heading(2, "Hello"))
  end

  def test_heading3
    assert_equal("### Hello",
                 @generator.heading(3, "Hello"))
  end

  def test_definition_list_item
    item = @generator.definition_list_item("Rabbit",
                                           "The presentation tool for Rubyist")
    assert_equal(<<-MARKDOWN, item)
Rabbit
:   The presentation tool for Rubyist
    MARKDOWN
  end

  def test_unordered_list_item
    assert_equal("* Hello",
                 @generator.unordered_list_item("Hello"))
  end

  def test_image
    image = @generator.image("lavie.png", :relative_height => 90)
    assert_equal(<<-EOR.rstrip, image)
![](lavie.png){:relative_height='90'}
EOR
  end

  def test_preformatted_line
    assert_equal("    Hello", @generator.preformatted_line("Hello"))
  end

  def test_comment
    assert_equal("", @generator.comment("Hello"))
  end
end
