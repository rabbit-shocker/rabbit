# Copyright (C) 2012-2024  Sutou Kouhei <kou@cozmixng.org>
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

require "rabbit/readme-parser"

class TestREADMEParser < Test::Unit::TestCase
  def setup
    @parser = Rabbit::READMEParser.new
    @title = "Theme benchmark"
    @description = <<-DESCRIPTION.strip
It's a slide for checking a Rabbit's theme. It contains many
elements. So it's useful for confirming your theme.

Please try to create your original theme!
    DESCRIPTION
  end

  private
  def assert_parse(content, extension=nil)
    Tempfile.create(["README", extension]) do |readme_file|
      readme_file.print(content)
      readme_file.close

      @parser.parse(readme_file.path)
      assert_equal({
                     :title       => @title,
                     :description => @description,
                   },
                   {
                     :title       => @parser.title,
                     :description => @parser.description,
                   })
    end
  end

  class TestRD < self
    private
    def readme_content
      <<-README
= #{@title}

#{@description}

== For author

=== Show

  rake
      README
    end

    class TestExtension < self
      def test_no_extension
        assert_parse(readme_content)
      end

      def test_rd
        assert_parse(readme_content, ".rd")
      end

      def test_rab
        assert_parse(readme_content, ".rab")
      end
    end
  end

  class TestHiki < self
    private
    def readme_content
      <<-README
! #{@title}

#{@description}

!! For author

!!! Show

  rake
      README
    end

    class TestExtension < self
      def test_no_extension
        assert_parse(readme_content)
      end

      def test_hiki
        assert_parse(readme_content, ".hiki")
      end
    end
  end

  class TestMarkdown < self
    private
    def readme_content
      <<-README
# #{@title}

#{@description}

## For author

### Show

  rake
      README
    end

    class TestExtension < self
      def test_no_extension
        assert_parse(readme_content)
      end

      def test_md
        assert_parse(readme_content, ".md")
      end
    end
  end

  class TestTextile < self
    private
    def readme_content
      <<-README
h1. #{@title}

#{@description}

h2. For author

h3. Show

  rake
      README
    end

    class TestExtension < self
      def test_no_extension
        assert_parse(readme_content)
      end

      def test_textile
        assert_parse(readme_content, ".textile")
      end
    end
  end
end
