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

require "rabbit/theme-configuration"

class TestThemeConfiguration < Test::Unit::TestCase
  def setup
    @theme = Rabbit::ThemeConfiguration.new
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
      "id"                => "clear-blue",
      "tags"              => ["blue"],
      "version"           => "1.0.2",
      "licenses"          => ["GPLv3+", "GFDL", "CC BY-SA 3.0"],
      "author"            => author_conf,
    }
    @theme.merge!(conf)
    assert_equal(conf, @theme.to_hash)
  end

  def test_default_version
    assert_equal("1.0.0", @theme.version)
  end
end
