# Copyright (C) 2014-2022  Sutou Kouhei <kou@cozmixng.org>
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

require "rabbit/logger"
require "rabbit/slide"
require "rabbit/source/memory"
require "rabbit/parser/markdown"

class RabbitParserMarkdownTest < Test::Unit::TestCase
  include Helper::Fixture
  include Helper::Parser

  private
  def parse(markdown)
    super(Rabbit::Parser::Markdown, markdown)
  end

  class TitlePageTest < self
    def parse(markdown)
      inspect_canvas(super(markdown))
    end

    def test_title
      assert_equal([
                     [
                       "TitleSlide", [
                         "Title", [
                           "Text",
                           "Title",
                         ],
                       ],
                     ],
                   ],
                   parse("# Title"))
    end
  end

  class BodyTest < self
    def parse(markdown)
      full_markdown = <<-MARKDOWN
\# Title

\# Page

#{markdown}
      MARKDOWN
      inspect_element(super(full_markdown)[1].body)
    end

    class TextEscapeTest < self
      def test_html_tag
        assert_equal([
                       "Body", [
                         "Paragraph", [
                           "Code", [
                             "Text",
                             "&#60;pre&#62;",
                           ],
                         ],
                       ],
                     ],
                     parse("`<pre>`"))
      end
    end

    class WaitTest < self
      def test_in_list_item
        assert_equal([
                       "Body", [
                         "ItemList", [
                           "ItemListItem", [
                             "Paragraph",
                             ["Text", "Hello"],
                             ["WaitTag", ""],
                             ["Text", " World"],
                           ],
                         ],
                       ],
                     ],
                     parse("* Hello{::wait/} World"))
      end
    end

    class DeletedTextTest < self
      def test_simple
        assert_equal([
                       "Body", [
                         "Paragraph",
                         ["Text", "Hello "],
                         ["DeletedText", ["Text", "deleted"]],
                         ["Text", " World"],
                       ],
                     ],
                     parse("Hello ~~deleted~~ World"))
      end

      def test_start_only
        assert_equal([
                       "Body", [
                         "Paragraph",
                         ["Text", "Hello ~~ World"],
                       ],
                     ],
                     parse("Hello ~~ World"))
      end

      def test_escaple
        assert_equal([
                       "Body", [
                         "Paragraph",
                         ["Text", "Hello ~~not deleted~~ World"],
                       ],
                     ],
                     parse("Hello \\~\\~not deleted\\~\\~ World"))
      end
    end

    class SyntaxHighlightTest < self
      def test_indent_lang
        assert_equal([
                       "Body", [
                         "SyntaxHighlightingBlock", [
                           "TextContainer",
                           [
                             "CustomTag", [
                               "SyntaxHighlightingText",
                               ["Text", "\"Hello World\""],
                             ],
                           ],
                         ],
                       ],
                     ],
                     parse(<<-MARKDOWN))
    "Hello World"
{: lang="ruby"}
                           MARKDOWN
      end

      def test_indent_language
        assert_equal([
                       "Body", [
                         "SyntaxHighlightingBlock", [
                           "TextContainer",
                           [
                             "CustomTag", [
                               "SyntaxHighlightingText",
                               ["Text", "\"Hello World\""],
                             ],
                           ],
                         ],
                       ],
                     ],
                     parse(<<-MARKDOWN))
    "Hello World"
{: language="ruby"}
                           MARKDOWN
      end

      def test_fence_kramdown
        assert_equal([
                       "Body", [
                         "SyntaxHighlightingBlock", [
                           "TextContainer",
                           [
                             "CustomTag", [
                               "SyntaxHighlightingText",
                               ["Text", "\"Hello World\""],
                             ],
                           ],
                         ],
                       ],
                     ],
                     parse(<<-MARKDOWN))
~~~ruby
"Hello World"
~~~
                           MARKDOWN
      end

      def test_fence_gfm
        assert_equal([
                       "Body", [
                         "SyntaxHighlightingBlock", [
                           "TextContainer",
                           [
                             "CustomTag", [
                               "SyntaxHighlightingText",
                               ["Text", "\"Hello World\""],
                             ],
                           ],
                         ],
                       ],
                     ],
                     parse(<<-MARKDOWN))
```ruby
"Hello World"
```
                           MARKDOWN
      end
    end

    class BlockDiagTest < self
      def test_codeblock_fence
        assert_equal([
                       "Body", [
                         "Image",
                         ""
                       ],
                     ],
                     parse(<<-MARKDOWN))
```blockdiag
{
  A -> B -> C;
}
```
                           MARKDOWN
      end
    end

    class ListTest < self
      class UnorderedListTest < self
        class NesetedTest < self
          def test_no_new_line_after_first_line
            assert_equal([
                           "Body", [
                             "ItemList", [
                               "ItemListItem", [
                                 "Paragraph", ["Text", "First"],
                               ], [
                                 "ItemList", [
                                   "ItemListItem", [
                                     "Paragraph", ["Text", "Second"],
                                   ],
                                 ],
                               ],
                             ],
                           ],
                         ],
                         parse(<<-MARKDOWN))
* First
  * Second
                         MARKDOWN
          end

          def test_have_new_line_after_first_line
            assert_equal([
                           "Body", [
                             "ItemList", [
                               "ItemListItem", [
                                 "Paragraph", ["Text", "First"],
                               ], [
                                 "ItemList", [
                                   "ItemListItem", [
                                     "Paragraph", ["Text", "Second"],
                                   ],
                                 ],
                               ],
                             ],
                           ],
                         ],
                         parse(<<-MARKDOWN))
* First

  * Second
                         MARKDOWN
          end
        end
      end
    end

    class ImageTest < self
      class InlineTest < self
        include Rabbit::GetText

        def test_unsupported
          image_path = fixture_path("image/png/lavie.png")
          message = _("![alt]{image} and other contents in a paragraph isn't supported: [:text, :img]")
          assert_raise(Rabbit::ParseError.new(message)) do
            parse(<<-MARKDOWN)
a ![](#{image_path})
            MARKDOWN
          end
        end
      end
    end

    class HorizontalRuleTest < self
      include Rabbit::GetText

      def test_unsupported
        message = _("horizontal rule isn't supported.")
        assert_raise(Rabbit::ParseError.new(message)) do
          parse(<<-MARKDOWN)
---
          MARKDOWN
        end
      end
    end

    class TagTest < self
      include Rabbit::GetText

      def test_no_name
        message = _("tag name is missing.")
        assert_raise(Rabbit::ParseError.new(message)) do
          parse(<<-MARKDOWN)
{::tag}content{:/tag}
          MARKDOWN
        end
      end

      def test_inline
        assert_equal([
                       "Body", [
                         "Paragraph", [
                           "CustomTag", [
                             "Text",
                             "Hello",
                           ],
                         ],
                       ],
                     ],
                     parse(<<-MARKDOWN))
{::tag name="x-large"}Hello{:/tag}
        MARKDOWN
      end

      def test_paragraph
        assert_equal([
                       "Body", [
                         "Paragraph", [
                           "CustomTag", [
                             "Text",
                             "Hello",
                           ],
                         ],
                       ],
                     ],
                     parse(<<-MARKDOWN))
{:.x-large}
Hello
        MARKDOWN
      end
    end
  end
end
