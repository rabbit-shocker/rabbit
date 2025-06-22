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

require "rabbit/source"
require "rabbit/logger"

class RabbitSourceTest < Test::Unit::TestCase
  class BaseTest < self
    class EncodingDetectionTest < self
      def test_euc_jp_like_utf8
        assert_equal(Encoding::UTF_8, guess_encoding("résumé"))
      end

      private
      def guess_encoding(string)
        source = Rabbit::Source::Memory.new(nil)
        source.source = string
        source.__send__(:guess_encoding, string)
      end
    end

    class ReadTest < self
      def test_binary
        pdf_header = +"%PDF-1.5\n%\xb5\xed\xae\xfb\n"
        pdf_header.force_encoding("ASCII-8BIT")
        assert_equal(pdf_header, read(pdf_header))
      end

      private
      def read(string)
        source = Rabbit::Source::Memory.new(string.encoding)
        source.source = string
        source.read
      end
    end
  end

  class ARGFTest < self
    def setup
      @input, @output = IO.pipe
      @source = Rabbit::Source::ARGF.new("UTF-8", @input)
    end

    def teardown
      @input.close
      @output.close
    end

    def test_base
      assert_equal(".", @source.base)
    end

    def test_full_path
      image = "sample.png"

      assert_equal(File.join(".", image), @source.full_path(image))
    end
  end

  class FileTest < self
    def setup
      @dir = File.dirname(__FILE__)
      @file = File.join(@dir, "sample.rd")
      FileUtils.touch(@file)
      @source = Rabbit::Source::File.new("UTF-8", @file)
    end

    def teardown
      FileUtils.rm_f(@file)
    end

    def test_base
      assert_equal(@dir, @source.base)
    end

    def test_full_path
      image = "sample.png"

      assert_equal(File.join(@dir, image), @source.full_path(image))
    end
  end

  class URITest < self
    def setup
      @base = "http://example.com/sample"
      @uri = "#{@base}/rabbit.rd"
      @source = Rabbit::Source::URI.new("UTF-8", @uri)
    end

    def test_base
      assert_equal(@base, @source.base)
    end

    def test_full_path
      image = "sample.png"

      assert_equal("#{@base}/#{image}", @source.full_path(image))
    end
  end
end
