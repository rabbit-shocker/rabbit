require "tempfile"

require 'rabbit/rabbit'
require 'rabbit/utils'
require 'rabbit/rt/rt2rabbit-lib'
require 'rabbit/ext/base'
require 'rabbit/ext/image'
require 'rabbit/ext/enscript'
require 'rabbit/tgif'

module Rabbit
  module Ext
    class BlockVerbatim < Base

      include SystemRunner
      include Image
      include Enscript
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
        return nil if prop['src'].nil?
        make_image(visitor, prop['src'], prop)
      end

      def ext_block_verb_enscript(label, source, content, visitor)
        return nil unless /^enscript (\w+)$/i =~ label
        lang = $1.downcase.untaint
        enscript_block(label, lang, source, content, visitor)
      end
      
      def ext_block_verb_tex(label, source, content, visitor)
        return nil unless /^TeX$/i =~ label
        src, prop = parse_source(source)
        src_file = Tempfile.new("rabbit")
        src_file.open
        src_file.print(src)
        src_file.close
        image_file = make_image_by_outer_command(src_file.path, prop, visitor)
        return nil if image_file.nil?
        image = make_image(visitor, %Q[file://#{image_file.path}], prop)
        image["_src"] = image_file # for protecting from GC
        image
      end

      def ext_block_verb_rt(label, source, content, visitor)
        return nil unless /^rt$/i =~ label
        rt_visitor = RT2RabbitVisitor.new(visitor)
        rt_visitor.visit(RT::RTParser.parse(content))
      end

      def ext_block_verb_block_quote(label, source, content, visitor)
        return nil unless /^blockquote$/i =~ label
        src, prop = parse_source(source)
        tree = RD::RDTree.new("=begin\n#{src}\n=end\n")
        elems = tree.root.children.collect do |child|
          child.accept(visitor)
        end
        BlockQuote.new(elems, prop)
      end
      
      private
      def make_image_by_outer_command(path, prop, visitor)
        begin
          make_image_by_tgif(path, prop, visitor)
        rescue ImageLoadError
          visitor.logger.warn($!.message)
          begin
            make_image_by_mimeTeX(path, prop, visitor)
          rescue ImageLoadError
            visitor.logger.warn($!.message)
            nil
          end
        end
      end
      
      def make_image_by_mimeTeX(path, prop, visitor)
        image_file = Tempfile.new("rabbit")
        command = ["mimetex.cgi", "-e", image_file.path, "-f", path]
        if run(*command)
          image_file
        else
          raise TeXCanNotHandleError.new(command.join(" "))
        end
      end

      def make_image_by_tgif(path, prop, visitor)
        Tgif.init
        image_file = Tempfile.new("rabbit")
        tgif_file = Tempfile.new("rabbit-tgif")
        obj_path = "#{tgif_file.path}.obj"
        eps_path = "#{tgif_file.path}.eps"
        File.open(path) do |f|
          src = []
          f.each_line do |line|
            src << line.chomp
          end
          src = normalize_src_for_tgif(src.join(" "))
          exp = parse_expression_for_tgif(src, prop, visitor.logger)
          exp.set_pos(prop['x'] || 100, prop['y'] || 100)
          tgif_file.open
          tgif_file.print(Tgif::TgifObject.preamble)
          tgif_file.print(exp.tgif_format)
          tgif_file.close
          begin
            command = ["tgif", "-print", "-eps", "-quiet", tgif_file.path]
            FileUtils.cp(tgif_file.path, obj_path)
            if run(*command)
              FileUtils.mv(eps_path, image_file.path)
              image_file
            else
              raise TeXCanNotHandleError.new(command.join(" "))
            end
          ensure
            FileUtils.rm_f(obj_path)
          end
        end
      end

      def normalize_src_for_tgif(src)
        src.gsub(/\\Large/, 'large').
          gsub(/\\Bigint_([^^]+)\^/, 'int \1 ').
          gsub(/\\/, '')
      end
      
      def parse_expression_for_tgif(src, prop, logger)
        token = Tgif::TokenList.new(src)
        exp = Tgif::Expression.from_token(token, nil, prop['color'], logger)
        if exp.is_a?(Tgif::TgifObject)
          exp
        else
          raise TeXCanNotHandleError.new(_("invalid source: %s") % src)
        end
      end
    end
  end
end
