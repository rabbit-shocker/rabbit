# Copyright (C) 2015-2019  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/logger"
require "rabbit/slide"
require "rabbit/source/memory"
require "rabbit/parser/wiki"

class RabbitParserWikiTest < Test::Unit::TestCase
  include Helper::Fixture
  include Helper::Parser

  private
  def parse(wiki_text)
    super(Rabbit::Parser::Wiki, wiki_text)
  end

  class ImageTest < self
    include Rabbit::GetText

    class InlineTest < self
      test "unsupported" do
        image_path = fixture_path("image/png/lavie.png")
        wiki_text = <<-WIKI
! Title

! Slide

a {{image(#{image_path.dump})}}
        WIKI
        message = _("inline {{image(...)}} isn't supported.")
        assert_raise(Rabbit::ParseError.new(message)) do
          parse(wiki_text)
        end
      end
    end

    class BlockTest < self
      test ":align => :right: twice" do
        image_path = fixture_path("image/png/lavie.png")
        wiki_text = <<-WIKI
! Title

! Slide

{{image(#{image_path.dump}, {:align => :right})}}

{{image(#{image_path.dump}, :align => :right)}}
        WIKI

        message = _("multiple {{image(..., :align => :right)}} isn't supported.")
        assert_raise(Rabbit::ParseError.new(message)) do
          parse(wiki_text)
        end
      end
    end
  end
end
