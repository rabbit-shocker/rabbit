require "test/unit"
require 'fileutils'

require "rabbit/source"

class RabbitSourceTest < Test::Unit::TestCase

  def setup
    @argf_input, @argf_output = IO.pipe
    @argf = Rabbit::Source::ARGF.new("UTF-8", @argf_input)

    @file_name = "test/sample.rd"
    @file_dir_name = File.dirname(@file_name)
    FileUtils.touch(@file_name)
    @file = Rabbit::Source::File.new("UTF-8", @file_name)

    @uri_name = "http://example.com/sample/rabbit.rd"
    @uri_base_name = File.dirname(@uri_name)
    @uri = Rabbit::Source::URI.new("UTF-8", @uri_name)
  end

  def teardown
    FileUtils.rm_f(@file_name)
  end
  
  def test_base
    assert_equal(".", @argf.base)
    assert_equal(File.dirname(@file_name), @file.base)

    base_uri = URI.parse(@uri_name)
    base_uri.path = File.dirname(base_uri.path)
    assert_equal(base_uri.to_s, @uri.base)
  end

  def test_full_path
    image = "sample.png"
    
    assert_equal(File.join(".", image), @argf.full_path(image))
    assert_equal(File.join(@file_dir_name, image), @file.full_path(image))
    assert_equal(File.join(@uri_base_name, image), @uri.full_path(image))
  end
  
end
