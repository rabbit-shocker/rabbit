# Copyright (C) 2014-2024  Sutou Kouhei <kou@cozmixng.org>
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

require "rabbit/slide"
require "rabbit/source/memory"
require "rabbit/parser/rd"

class RabbitParserRDTest < Test::Unit::TestCase
  class MatchTest < self
    def match?(source)
      Rabbit::Parser::RD.match?(source)
    end

    class ContentTest < self
      def match?(content)
        source = Rabbit::Source::Memory.new("UTF-8", nil)
        source.source = content
        super(source)
      end

      def test_with_space
        assert_true(match?(+"= Hello"))
      end

      def test_no_space
        assert_true(match?(+"=Hello"))
      end
    end
  end
end
