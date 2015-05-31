# Copyright (C) 2004-2015  Kouhei Sutou <kou@cozmixng.org>
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

require "tempfile"

require "rabbit/rabbit"
require "rabbit/utils"
require "rabbit/parser/rd/rt/rt2rabbit-lib"
require "rabbit/parser/rd/ext/base"
require "rabbit/parser/rd/ext/image"
require "rabbit/parser/rd/ext/video"
require "rabbit/parser/ext/enscript"
require "rabbit/parser/ext/tex"
require "rabbit/parser/ext/aafigure"
require "rabbit/parser/ext/blockdiag"
require "rabbit/parser/ext/coderay"
require "rabbit/parser/ext/emacs"

module Rabbit
  module Parser
    class RD
      module Ext
        class BlockVerbatim < Base
          include Image
          include Video
          include GetText

          def default_ext_block_verbatim(label, source, content, visitor)
            content = visitor.apply_to_String(content.rstrip)
            text = Text.new(content)
            PreformattedBlock.new(PreformattedText.new(text))
          end

          def ext_block_verb_quote(label, source, content, visitor)
            return nil unless /^_$/i =~ label
            default_ext_block_verbatim("", source, source, visitor)
          end

          def ext_block_verb_img(label, source, content, visitor)
            return nil unless /^(?:image|img)$/i =~ label
            src, prop = parse_source(source)
            return nil if prop["src"].nil?

            if prop["align"] == "right"
              body = visitor.current_body
              if body["background-image"]
                raise ParseError,
                      _("multiple 'align = right' isn't supported.")
              end
              prop.each do |name, value|
                name = name.gsub(/_/, "-")
                if name == "src"
                  property_name = "background-image"
                else
                  property_name = "background-image-#{name}"
                end
                body[property_name] = value
              end
              :no_element
            else
              make_image(visitor, prop["src"], prop)
            end
          end

          def ext_block_verb_video(label, source, content, visitor)
            return nil unless /^video$/i =~ label
            src, prop = parse_source(source)
            return nil if prop["src"].nil?

            make_video(visitor, prop["src"], prop)
          end

          def ext_block_verb_enscript(label, source, content, visitor)
            return nil unless /^enscript (\w+)$/i =~ label
            lang = $1.downcase.untaint

            src, prop = parse_source(source)
            logger = visitor.logger

            result = nil
            if Parser::Ext::Enscript.check_availability(lang, logger)
              result = Parser::Ext::Enscript.highlight(lang, src, logger)
            end
            result || default_ext_block_verbatim(label, src, src, visitor)
          end

          def ext_block_verb_LaTeX(label, source, content, visitor)
            return nil unless /^LaTeX$/i =~ label
            make_image_from_file(source, visitor) do |src_file_path, prop|
              Parser::Ext::TeX.make_image_by_LaTeX(src_file_path, prop, visitor)
            end
          end

          def ext_block_verb_mimeTeX(label, source, content, visitor)
            return nil unless /^mimeTeX$/i =~ label
            make_image_from_file(source, visitor) do |src_file_path, prop|
              Parser::Ext::TeX.make_image_by_mimeTeX(src_file_path, prop,
                                                     visitor)
            end
          end

          def ext_block_verb_aafigure(label, source, content, visitor)
            return nil unless /^aafigure$/i =~ label
            make_image_from_file(source, visitor) do |src_file_path, prop|
              Parser::Ext::AAFigure.make_image(src_file_path, prop, visitor)
            end
          end

          def ext_block_verb_blockdiag(label, source, content, visitor)
            return nil unless /^blockdiag$/i =~ label
            make_image_from_file(source, visitor) do |src_file_path, prop|
              Parser::Ext::BlockDiag.make_image(src_file_path, prop, visitor)
            end
          end

          def ext_block_verb_coderay(label, source, content, visitor)
            return nil unless /^coderay (\w+)$/i =~ label
            lang = $1.downcase.untaint

            src, prop = parse_source(source)
            logger = visitor.logger

            result = Parser::Ext::CodeRay.highlight(lang, src, logger)
            result || default_ext_block_verbatim(label, src, src, visitor)
          end

          def ext_block_verb_emacs(label, source, content, visitor)
            return nil unless /^emacs(?:\s+(.+))?$/i =~ label
            mode_line = $1.untaint

            src, prop = parse_source(source)
            logger = visitor.logger

            result = Parser::Ext::Emacs.highlight(src, logger, mode_line)
            result || default_ext_block_verbatim(label, src, src, visitor)
          end

          def ext_block_verb_rt(label, source, content, visitor)
            return nil unless /^rt$/i =~ label
            unless defined?(RT2RabbitVisitor)
              visitor.logger.warn(_("RTtool isn't available"))
              return nil
            end
            rt_visitor = RT2RabbitVisitor.new(visitor)
            rt_visitor.visit(RT::RTParser.parse(content))
          end

          def ext_block_verb_block_quote(label, source, content, visitor)
            return nil unless /^block[_-]?quote$/i =~ label
            src, prop = parse_source(source)
            tree = ::RD::RDTree.new("=begin\n#{src}\n=end\n")
            elems = tree.root.children.collect do |child|
              child.accept(visitor)
            end
            BlockQuote.new(elems, prop)
          end

          def ext_block_verb_wait(label, source, content, visitor)
            return nil unless /^wait$/i =~ label

            src, prop = parse_source(source)
            tree = ::RD::RDTree.new("=begin\n#{src}\n=end\n")

            wait_block = WaitBlock.new
            visitor.register_pause(wait_block)

            tree.root.children.each do |child|
              wait_block << child.accept(visitor)
            end
            wait_block
          end
        end
      end
    end
  end
end
