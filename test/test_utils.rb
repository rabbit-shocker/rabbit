require "rabbit-test-utils"

require "rabbit/utils"

class RabbitUtilsTest < Test::Unit::TestCase

  def test_rotate_pixbuf
    color_space = Gdk::Pixbuf::COLORSPACE_RGB
    has_alpha = true
    bit_per_samples = 8
    byte_per_samples = bit_per_samples / 8
    
    orig_data = [
      [[0, 0, 0, 0], [1, 1, 1, 1], [2, 2, 2, 2]],
      [[3, 3, 3, 3], [4, 4, 4, 4], [5, 5, 5, 5]],
      [[6, 6, 6, 6], [7, 7, 7, 7], [8, 8, 8, 8]],
      [[9, 9, 9, 9], [10, 10, 10, 10], [11, 11, 11, 11]],
      [[12, 12, 12, 12], [13, 13, 13, 13], [14, 14, 14, 14]],
    ]
    orig_pixels = data_to_pixels(orig_data)
    orig_width = orig_data[0].size
    orig_height = orig_data.size
    orig_channels = orig_data[0][0].size
    orig_row_stride = orig_width * orig_channels * byte_per_samples
    
    expected_data = [
      [[12, 12, 12, 12], [9, 9, 9, 9], [6, 6, 6, 6], [3, 3, 3, 3], [0, 0, 0, 0]],
      [[13, 13, 13, 13], [10, 10, 10, 10], [7, 7, 7, 7], [4, 4, 4, 4], [1, 1, 1, 1]],
      [[14, 14, 14, 14], [11, 11, 11, 11], [8, 8, 8, 8], [5, 5, 5, 5], [2, 2, 2, 2]],
    ]
    expected_pixels = data_to_pixels(expected_data)
    expected_width = expected_data[0].size
    expected_height = expected_data.size
    expected_channels = expected_data[0][0].size
    expected_row_stride = expected_width * expected_channels * byte_per_samples

    orig_pixbuf = Gdk::Pixbuf.new(orig_pixels, color_space,
                                  has_alpha, bit_per_samples,
                                  orig_width, orig_height,
                                  orig_row_stride)

    actual_pixbuf = Rabbit::Utils.rotate_pixbuf(orig_pixbuf)

    assert_equal(expected_width, actual_pixbuf.width)
    assert_equal(expected_height, actual_pixbuf.height)
    assert_equal(expected_channels, actual_pixbuf.n_channels)
    assert_equal(expected_row_stride, actual_pixbuf.rowstride)
    assert_equal(expected_pixels, actual_pixbuf.pixels)
  end

  def test_data_to_pixels
    data = [
      [[0, 0, 0, 0], [1, 1, 1, 1], [2, 2, 2, 2]],
      [[3, 3, 3, 3], [4, 4, 4, 4], [5, 5, 5, 5]],
      [[6, 6, 6, 6], [7, 7, 7, 7], [8, 8, 8, 8]],
      [[9, 9, 9, 9], [10, 10, 10, 10], [11, 11, 11, 11]],
      [[12, 12, 12, 12], [13, 13, 13, 13], [14, 14, 14, 14]],
    ]
    pixels = "\0" * data.size * data[0].size * 4
    n_channels = data[0][0].size
    15.times do |i|
      base = i * n_channels
      n_channels.times do |n|
        pixels[base + n] = i
      end
    end
    assert_equal(pixels, data_to_pixels(data))
  end

  private
  def data_to_pixels(data)
    data.collect do |row|
      row.collect do |bits|
        ret = "\0" * bits.size
        bits.each_with_index do |bit, i|
          ret[i] = bit
        end
        ret
      end.join("")
    end.join("")
  end
  
end
