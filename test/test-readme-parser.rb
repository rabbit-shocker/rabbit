# Copyright (C) 2012-2013  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/readme-parser"

class TestREADMEParser < Test::Unit::TestCase
  def setup
    @parser = Rabbit::READMEParser.new
  end

  def test_rd
    title = "Theme benchmark"
    description = <<-EOD.strip
It's a slide for checking a Rabbit's theme. It contains many
elements. So it's useful for confirming your theme.

Please try to create your original theme!
EOD

    assert_parse(title, description, <<-EOR)
= #{title}

#{description}

== For author

=== Show

  rake
EOR
  end

  private
  def assert_parse(title, description, content)
    stub(File).read("README") {content}
    @parser.parse("README")
    assert_equal({
                   :title       => title,
                   :description => description,
                 },
                 {
                   :title       => @parser.title,
                   :description => @parser.description,
                 })
  end
end
