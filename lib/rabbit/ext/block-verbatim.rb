require "tempfile"

require 'rabbit/utils'
require 'rabbit/rt/rt2rabbit-lib'
require 'rabbit/ext/base'
require 'rabbit/ext/image'

module Rabbit
  module Ext
    class BlockVerbatim < Base

      include SystemRunner
      include Image
      
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
        image_file = nil
        begin
          image_file = make_image_by_mimeTeX(src_file.path)
        rescue ImageLoadError
          visitor.logger.warn($!.message)
          return nil
        end
        prop['src'] = %Q[file://#{image_file.path}]
        prop['_src'] = image_file # for protecting from GC
        make_image(visitor, prop['src'], prop)
      end

      def ext_block_verb_rt(label, content, visitor)
        return nil unless /^rt$/i =~ label
        @rt_visitor = RT2RabbitVisitor.new(visitor)
        @rt_visitor.visit(RT::RTParser.parse(content))
      end
      
      private
      def make_image_by_mimeTeX(path)
        image_file = Tempfile.new("rabbit")
        command = ["mimetex.cgi", "-e", image_file.path, "-f", path]
        if run(*command)
          image_file
        else
          raise TeXCanNotHandleError.new(command.join(" "))
        end
      end

    end
  end
end
