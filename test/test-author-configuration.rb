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

require "rabbit/author-configuration"

class TestAuthorConfiguration < Test::Unit::TestCase
  def setup
    @author = Rabbit::AuthorConfiguration.new
  end

  def test_merge!
    conf = {
      "markup_language"    => "rd",
      "name"               => "Kouhei Sutou",
      "email"              => "kou@cozmixng.org",
      "rubygems_user"      => "kou",
      "slideshare_user"    => "kou",
      "speaker_deck_user"  => "kou",
    }
    @author.merge!(conf)
    assert_equal(conf, @author.to_hash)
  end
end
