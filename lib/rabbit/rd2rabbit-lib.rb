require "forwardable"

require "rd/rdvisitor"
require "rd/version"

require "rabbit/element"
require "rabbit/ext/refer"
require "rabbit/ext/inline-verbatim"
require "rabbit/ext/block-verbatim"

module Rabbit
  class RD2RabbitVisitor < RD::RDVisitor
    extend Forwardable
    
    include RD::MethodParse
    include Element
    
    SYSTEM_NAME = "RD2RabbitLVisitor"
    SYSTEM_VERSION = "0.0.1"
    VERSION = RD::Version.new_from_version_string(SYSTEM_NAME, SYSTEM_VERSION)

    # must-have constants
    OUTPUT_SUFFIX = ""
    INCLUDE_SUFFIX = ["rabbit", "rb"]

    def self.version
      VERSION
    end
    
    METACHAR = { "<" => "&#60;", ">" => "&#62;", "&" => "&#38;" }

    EXTENSIONS = {
      "refer" => Ext::Refer,
      "inline_verbatim" => Ext::InlineVerbatim,
      "block_verbatim" => Ext::BlockVerbatim,
    }

    def_delegators(:@canvas, :logger, :full_path, :tmp_dir_name)

    
    def initialize(canvas)
      @canvas = canvas
      @title_slide = false

      @slides = []
      @slide = nil
      @footnotes = []
      @foottexs = []
      @index = {}

      init_extensions
      super()
    end
    
    def visit(tree)
      prepare_labels(tree, "label-")
      prepare_footnotes(tree)
      super(tree)
    end

    def apply_to_DocumentElement(element, contents)
      target = nil
      display = false
      contents.each do |content|
        display = false if content.nil?
        case content
        when nil
          # ignore
        when Slide
          target = Body.new
          content << target
          @canvas << content
          display = true
        when TitleSlide
          target = content
          @canvas << content
          display = true
        else
          target << content if display
        end
      end
      burn_out_foottexts
    end

    def apply_to_Headline(element, title)
      anchor = get_anchor(element)
      if element.level == 1
        if @slides.empty?
          @title_slide = true
          @slide = TitleSlide.new(Title.new(title))
        else
          @title_slide = false
          @slide = Slide.new(HeadLine.new(title))
        end
        @foottexts << []
        @slides << @slide
        @slide
      else
        @slide = nil
        nil
      end
    end

    def apply_to_Include(element)
      paths = element.tree.include_paths
      fname = search_file(element.filename, paths, @include_suffix)
      if fname
        File.open(fname) do |f|
          instance_eval(f.read, fname, 0)
        end
      end
    end

    def apply_to_TextBlock(element, content)
      Paragraph.new(content)
    end

    def apply_to_Verbatim(element)
      content = []
      element.each_line do |line|
        content << line
      end
      content_str = content.join("")
      /\A#\s*([^\n]+)\s*(?:\n.*)?\z/m =~ content_str
      apply_to_extension("block_verbatim", $1, content_str)
    end
  
    def apply_to_ItemList(element, items)
      apply_to_List(element, items, ItemList)
    end
  
    def apply_to_EnumList(element, items)
      i = 1
      apply_to_List(element, items, EnumList) do |list, item|
        item.order = i
        i += 1
      end
    end
    
    def apply_to_DescList(element, items)
      apply_to_List(element, items, DescriptionList)
    end

    def apply_to_MethodList(element, items)
      apply_to_List(element, items, MethodList)
    end

    def apply_to_List(element, items, klass)
      list = klass.new()
      items.each do |item|
        list << item
        if block_given?
          yield(list, item)
        end
      end
      list
    end
    
    def apply_to_ItemListItem(element, content)
      apply_to_ListItem(element, content, ItemListItem.new)
    end
    
    def apply_to_EnumListItem(element, content)
      apply_to_ListItem(element, content, EnumListItem.new)
    end

    def apply_to_DescListItem(element, term, description)
      desc_term = DescriptionTerm.new(term)
      item = DescriptionListItem.new(desc_term)
      apply_to_ListItem(element, description, item)
    end

    def apply_to_MethodListItem(element, term, description)
      method_term = parse_method(term)
      item = MethodListItem.new(method_term)
      apply_to_ListItem(element, description, item)
    end
  
    def apply_to_ListItem(element, contents, item)
      contents.each do |content|
        item << content
      end
      item
    end

    def apply_to_StringElement(element)
      content = element.content.gsub(/\n\s*/, '')
      NormalText.new(apply_to_String(content))
    end

    def apply_to_Emphasis(element, content)
      create_have_text_element(Emphasis, content)
    end
  
    def apply_to_Code(element, content)
      create_have_text_element(Code, content)
    end
  
    def apply_to_Var(element, content)
      create_have_text_element(Veriable, content)
    end
  
    def apply_to_Keyboard(element, content)
      create_have_text_element(Keybord, content)
    end
  
    def apply_to_Index(element, content)
      create_have_text_element(Index, content)
    end

    def apply_to_Reference_with_RDLabel(element, content)
      apply_to_extension("refer", element.label, content)
    end

    def apply_to_Reference_with_URL(element, content)
      ref = create_have_text_element(ReferText, content)
      ref.to = element.label.url
      ref
    end

    def apply_to_Footnote(element, content)
      if @slide.nil?
        NormalText.new("")
      else
        num = get_footnote_num(element)
        raise ArgumentError, "[BUG?] #{element} is not registered." unless num
        add_foottext(num, content)
        Footnote.new(num)
      end
    end

    def apply_to_Foottext(element, content)
      num = get_footnote_num(element)
      raise ArgumentError, "[BUG] #{element} isn't registered." unless num
      Foottext.new(num, content)
    end

    def apply_to_Verb(element)
      content = apply_to_String(element.content)
      apply_to_extension("inline_verbatim", element.to_label, content)
    end

    def apply_to_String(str)
      meta_char_escape(str)
    end

    private
    def init_extensions
      @installed_extensions = {}
      EXTENSIONS.each do |name, klass|
        @installed_extensions[name] =  klass.new
      end
    end

    def prepare_footnotes(tree)
      @footnotes = tree.find_all{|i| i.is_a? RD::Footnote }
      @foottexts = []
    end

    def get_footnote_num(fn)
      unless fn.is_a? RD::Footnote
        raise ArgumentError, "#{fn} must be Footnote."
      end
      i = @footnotes.index(fn)
      if i
        i + 1
      else
        nil
      end
    end

    def parse_method(method)
      klass, kind, method, args = RD::MethodParse.analize_method(method)
      
      term = MethodTerm.new

      if kind == :function
        klass = kind = nil
      else
        kind = RD::MethodParse.kind2str(kind)
      end
      
      case method
      when "self"
        klass, kind, method, args = RD::MethodParse.analize_method(args)

        term = make_pre_inited_method_term(klass, kind)
        term << MethodName.new("self")
        add_args_to_method_term(term, args)
        term
      when "[]"
        args.strip!
        args.sub!(/^\((.*)\)$/, '\\1')
        args.split(/,/)
        term = make_pre_inited_method_term(klass, kind)
        term << MethodName.new("[")
        args.each do |arg|
          term << Variable.new(arg)
        end
        term << MethodName.new("] = ")
        term
      when "[]="
        args.tr!(' ', '')
        args.sub!(/^\((.*)\)$/, '\\1')
        ary = args.split(/,/)
        
        case ary.length
        when 1
          args = []
          val = 'val'
        when 2
          args, val = *ary
          args = [Variable.new(args)]
        when 3
          args = ary[0, 2].inject([]) do |result, x|
            result + [Variable.new(x), Code.new(", ")]
          end
          args.pop # removed too much ", "
          val = ary[2]
        end
        
        term = make_pre_inited_method_term(klass, kind)
        term << MethodName.new("[")
        args.each do |arg|
          term << arg
        end
        term << MethodName.new("] = ")
        term << Variable.new(val)
        term
      else
        term = make_pre_inited_method_term(klass, kind)
        term << MethodName.new(method)
        term.name = method
        add_args_to_method_term(term, args)
        term
      end
    end

    def make_pre_inited_method_term(klass, kind)
      term = MethodTerm.new
      term << ClassName.new(klass) if klass
      term << MethodKind.new(kind) if kind
      term
    end

    def add_args_to_method_term(term, args)
      first = true
      prev_index = 0
      args.scan(/(?:&#?)?\w+;?/) do |m|
        code = $PREMATCH[prev_index..-1]
        term << Code.new(code) unless code.empty?
        prev_index = $PREMATCH.size + m.size
        if /(?:&#?)\w+;/ =~ m
          term << Code.new(m)
        else
          term << Variable.new(m)
        end
      end
      term << Code.new($POSTMATCH) if $POSTMATCH
      term
    end

    def meta_char_escape(str)
      str.gsub(/[<>&]/) do
        METACHAR[$&]
      end
    end

    def add_foottext(num, foottext)
      unless @footnotes[num - 1]
        raise ArgumentError, "[BUG] footnote ##{num} isn't here."
      end
      @foottexts.last << [foottext, num - 1]
    end
    
    def burn_out_foottexts
      @slides.each do |slide|
        ftb = FoottextBlock.new
        current_foottexts = @foottexts.shift
        while ft_info = current_foottexts.shift
          ft, num = ft_info
          ftb << apply_to_Foottext(@footnotes[num], ft)
        end
        slide << ftb
      end
    end

    def title_slide?
      @title_slide
    end

    def apply_to_extension(ext_type, label, content)
      result = nil
      if @installed_extensions.has_key?(ext_type)
        result = @installed_extensions[ext_type].apply(label, content, self)
      end
      if result.nil? and
          respond_to?("default_ext_#{ext_type}", true)
        result = __send__("default_ext_#{ext_type}", label, content)
      end
      if result.nil?
        logger.error("[BUG] [#{label}] #{ext_type} extension isn't installed.")
      end
      result
    end

    def default_ext_refer(label, content)
      ref = create_have_text_element(ReferText, content)
      ref.to = label.element_label
      ref
    end

    def default_ext_inline_verbatim(label, content)
      Verbatim.new(content)
    end

    def default_ext_block_verbatim(label, content)
      content = apply_to_String(content)
      PreformattedBlock.new([PreformattedText.new(content)])
    end

    def create_have_text_element(klass, content)
      raise "Why???" if content.size > 1
      klass.new(content.collect{|x| x.text}.join(""))
    end
    
  end
end
