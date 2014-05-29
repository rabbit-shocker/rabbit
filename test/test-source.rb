# -*- coding: utf-8 -*-

require 'fileutils'

require "rabbit-test-utils"

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
        logger = Rabbit::Logger::STDERR.new
        source = Rabbit::Source::Memory.new(nil, logger)
        source.source = string
        source.__send__(:guess_encoding, string)
      end
    end
  end

  class ARGFTest < self
    def setup
      logger = Rabbit::Logger::STDERR.new

      @input, @output = IO.pipe
      @source = Rabbit::Source::ARGF.new("UTF-8", logger, @input)
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
      logger = Rabbit::Logger::STDERR.new

      @dir = File.dirname(__FILE__)
      @file = File.join(@dir, "sample.rd")
      FileUtils.touch(@file)
      @source = Rabbit::Source::File.new("UTF-8", logger, @file)
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
      logger = Rabbit::Logger::STDERR.new

      @base = "http://example.com/sample"
      @uri = "#{@base}/rabbit.rd"
      @source = Rabbit::Source::URI.new("UTF-8", logger, @uri)
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
