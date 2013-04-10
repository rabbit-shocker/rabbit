# Copyright (C) 2013 Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/slideshare"
require "rabbit/logger"

class TestSlideShare < Test::Unit::TestCase
  def setup
    logger = Rabbit::Logger::STDERR.new
    @slideshare = Rabbit::Task::SlideShare.new(logger)
  end

  def test_upload
    slideshow_id = 12345678
    url = "rabbit_id-#{slideshow_id}"
    mock(@slideshare).upload_slide {slideshow_id}
    mock(@slideshare).edit_title(slideshow_id)
    mock(@slideshare).slide_url(slideshow_id) {url}
    assert_equal(url, @slideshare.upload)
  end
end
