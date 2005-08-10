require "tempfile"

require 'rabbit/rabbit'
require 'rabbit/utils'
require 'rabbit/rt/rt2rabbit-lib'
require 'rabbit/ext/base'
require 'rabbit/ext/image'
require 'rabbit/tgif'

module Rabbit
  module Ext
    class BlockVerbatim < Base

      include SystemRunner
      include Image
      include GetText

      def ext_block_verb_quote(label, content, visitor)
        return nil unless /^_$/i =~ label
        content.sub!(/\A[^\n]*\n/, '')
        visitor.__send__(:default_ext_block_verbatim, "", content)
      end

      def ext_block_verb_img(label, content, visitor)
        return nil unless /^(?:image|img)$/i =~ label
        prop = {}
        content.each do |line|
          if /^(?:#\s*)?(\S+)\s*=\s*(.+)\s*$/ =~ line
            prop[$1] = $2
          end
        end
        return nil if prop['src'].nil?
        make_image(visitor, prop['src'], prop)
      end

      def ext_block_verb_tex(label, content, visitor)
        return nil unless /^TeX$/i =~ label
        prop = {}
        src_file = Tempfile.new("rabbit")
        in_src = false
        src_file.open
        content.each do |line|
          if in_src
            src_file.print(line)
          else
            if /^\s*$/ =~ line
              in_src = true
            elsif /^(?:#\s*)?(\S+)\s*=\s*(.+)\s*$/ =~ line
              prop[$1] = $2
            end
          end
        end
        src_file.close
        image_file = make_image_by_outer_command(src_file.path, prop, visitor)
        return nil if image_file.nil?
        image = make_image(visitor, %Q[file://#{image_file.path}], prop)
        image["_src"] = image_file # for protecting from GC
        image
      end

      def ext_block_verb_rt(label, content, visitor)
        return nil unless /^rt$/i =~ label
        @rt_visitor = RT2RabbitVisitor.new(visitor)
        @rt_visitor.visit(RT::RTParser.parse(content))
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
            FileUtils.ln_sf(tgif_file.path, obj_path)
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
