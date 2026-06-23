# Copyright (C) 2012-2022  Sutou Kouhei <kou@cozmixng.org>
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

require "rabbit/slide-configuration"

class TestSlideConfiguration < Test::Unit::TestCase
  def setup
    @slide = Rabbit::SlideConfiguration.new
  end

  def test_merge!
    author_conf = {
      "markup_language"    => nil,
      "name"               => "Kouhei Sutou",
      "email"              => "kou@cozmixng.org",
      "rubygems_user"      => "kou",
      "slideshare_user"    => "kou",
      "speaker_deck_user"  => "kou",
    }
    conf = {
      "id"                => "sprk2012",
      "base_name"         => "how-to-make-clear-code",
      "tags"              => ["rabbit", "sprk2012", "clear code"],
      "presentation_date" => "2012-09-16",
      "presentation_start_time" => "2012-09-16T10:00:00+0900",
      "presentation_end_time" => "2012-09-16T10:30:00+0900",
      "version"           => "2012.09.16.0",
      "licenses"          => ["GPLv3+", "GFDL", "CC BY-SA 3.0"],
      "slideshare_id"     => "rabbit-14073776",
      "source_code_uri"   => "https://example.com/source",
      "speaker_deck_id"   => "rabbit-debian",
      "vimeo_id"          => "00000000",
      "youtube_id"        => "XXXXX-xxxxx",
      "author"            => author_conf,
      "width"             => 800,
      "height"            => 600,
      "markup_language"   => "rd",
      "readme_markup_language" => nil,
    }
    @slide.id = "RubyKaigi2012"
    @slide.merge!(conf)

    object_conf = conf.dup
    object_conf["presentation_date"] = Date.parse(conf["presentation_date"])
    object_conf["presentation_start_time"] =
      Time.parse(conf["presentation_start_time"])
    object_conf["presentation_end_time"] =
      Time.parse(conf["presentation_end_time"])
    assert_equal(object_conf,
                 @slide.to_hash)
  end

  class TestDefaultVersion < self
    def test_have_presentation_date
      @slide.presentation_date = "2012-09-16"
      assert_equal("2012.9.16.0", @slide.version)
    end

    def test_no_presentation_date
      assert_equal("1.0.0", @slide.version)
    end
  end
end
