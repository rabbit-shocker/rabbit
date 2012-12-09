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

require "rabbit/slide-configuration"

class TestSlideConfiguration < Test::Unit::TestCase
  def setup
    @slide = Rabbit::SlideConfiguration.new
  end

  def test_merge!
    author_conf = {
      "markup_language"    => "rd",
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
      "presentation_date" => ["2012/09/16"],
      "version"           => "2012.09.16",
      "licenses"          => ["GPLv3+", "GFDL", "CC BY-SA 3.0"],
      "slideshare_id"     => "rabbit-14073776",
      "speaker_deck_id"   => "rabbit-debian",
      "ustream_id"        => "25451894",
      "vimeo_id"          => "00000000",
      "author"            => author_conf,
    }
    @slide.id = "RubyKaigi2012"
    @slide.merge!(conf)
    assert_equal(conf, @slide.to_hash)
  end

  class TestDefaultVersion < self
    def test_have_presentation_date
      @slide.presentation_date = "2012/09/16"
      assert_equal("2012.09.16", @slide.version)
    end

    def test_no_presentation_date
      assert_equal("1.0.0", @slide.version)
    end
  end
end
