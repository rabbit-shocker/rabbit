# based on tgif.rb in http://homepage2.nifty.com/aito/tgiftools/
#   (C) Copyright 1998 by Akinori Ito
#
# License:
#
# This software may be redistributed freely for this purpose, in full 
# or in part, provided that this entire copyright notice is included 
# on any copies of this software and applications and derivations thereof.
#
# This software is provided on an "as is" basis, without warranty of any
# kind, either expressed or implied, as to any matter including, but not
# limited to warranty of fitness of purpose, or merchantability, or
# results obtained from use of this software.
#
#
# modified by Kouhei Sutou <kou@cozmixng.org>

require 'thread'

module Rabbit
  module Tgif

    @@default_size = 30 #point
    @@debug_output = false
    @@default_color = 'black'

    @@mutex = Mutex.new
    @@initialized = false
    
    class << self
      def default_size
        @@default_size
      end

      def default_size=(val)
        @@default_size = val
      end

      def debug_output
        @@debug_output
      end

      def debug_output=(val)
        @@debug_output = val
      end

      def default_color
        @@default_color
      end

      def default_color=(val)
        @@default_color = val
      end

      def init
        @@mutex.synchronize do
          unless @@initialized
            TextChar.init
            @@initialized = true
          end
        end
      end
    end

    class Error < StandardError; end
    
class TokenList
  def isalpha(c)
   c.kind_of?(Fixnum) and ((?A <= c and c <= ?Z) or (?a <= c and c <= ?z))
  end
  def isdigit(c)
   c.kind_of?(Fixnum) and (?0 <= c and c <= ?9)
  end
  def initialize(str)
    @list = []
    i = 0
    while i < str.size
      c = str[i]
      if isalpha(c) then
        j = i
        begin
          i += 1
        end until !isalpha(str[i])
        @list.push(str[j,i-j])
      elsif isdigit(c) then
        j = i
        begin
          i += 1
        end until !isdigit(str[i])
        @list.push(str[j,i-j])
      elsif c == ?# then
        i += 1
        unless isdigit(str[i])
          raise Error, "Bad token: \# not followed by number"
        end
        @list.push(str[i].chr.to_i)
        i += 1
      elsif c == ?{ then
        nbrace = 0
        j = i
        begin
          i += 1
          if str[i] == ?{ then
            nbrace += 1
          elsif str[i] == ?} then
            nbrace -= 1
          elsif str[i].nil? then
            raise Error, "unterminated brace"
          end
        end until nbrace < 0
        @list.push(TokenList.new(str[j+1,i-j-1]))
        i += 1
      elsif c == ?" then
        j = i
        i += 1
        while i < str.size
          if str[i] == ?\\ then
             i += 1
          elsif str[i] == ?" then
             i += 1
             break
          end
          i += 1
        end
        @list.push(str[j,i-j])
      elsif c != ?\  and c != ?\t and c != ?\n then
        @list.push(c.chr)
        i += 1
      else
        i += 1
      end
    end
  end
  def each
    for x in @list
      yield x
    end
  end
  def [](i)
    @list[i]
  end
  def size
    @list.size
  end
  def push(tok)
    @list.push(tok)
  end
  protected :push
  def apply_arg(arg)
    n = TokenList.new('')
    for p in @list
      case p
      when String
        n.push p
      when TokenList
        n.push(p.apply_arg(arg))
      when Fixnum
        n.push(arg[p-1])
      end
    end
    n
  end
  def flatten
    res = []
    for t in @list
      if t.kind_of?(String) then
        res.push(t)
      else
        res.push('{')
        res.push(*t.flatten)
        res.push('}')
      end
    end
    res
  end
  def to_s
    flatten.join(' ')
  end
end

class Offset
  def initialize(x,y)
    @x = x
    @y = y
  end
  def add(x,y)
    @x += x
    @y += y
  end
end

class RelativeOffset < Offset
  def instanciate(xabs,yabs)
    [@x*xabs,@y*yabs]
  end
end

class AbsoluteOffset < Offset
  def instanciate(xabs,yabs)
    [@x,@y]
  end
end

module Utils
def size_script(size)
  (size*0.6).to_i
end

def size_large(size)
  (size*1.5).to_i
end
end

class TgifObject
  include Utils
  ID = [0]
  PREAMBLE = '%TGIF 3.0-p16
state(0,33,100.000,0,0,0,16,1,9,1,1,0,0,1,0,1,1,\'Times-Roman\',0,30,0,0,0,10,0,0,1,1,0,16,0,0,1,1,1,0,1088,1408,0,0,2880).
%
% @(#)$Header$
% %W%
%
unit("1 pixel/pixel").
color_info(11,65535,0,[
	"magenta", 65535, 0, 65535, 65535, 0, 65535, 1,
	"red", 65535, 0, 0, 65535, 0, 0, 1,
	"green", 0, 65535, 0, 0, 65535, 0, 1,
	"blue", 0, 0, 65535, 0, 0, 65535, 1,
	"yellow", 65535, 65535, 0, 65535, 65535, 0, 1,
	"pink", 65535, 49344, 52171, 65535, 49344, 52171, 1,
	"cyan", 0, 65535, 65535, 0, 65535, 65535, 1,
	"CadetBlue", 24415, 40606, 41120, 24415, 40606, 41120, 1,
	"white", 65535, 65535, 65535, 65535, 65535, 65535, 1,
	"black", 0, 0, 0, 0, 0, 0, 1,
	"DarkSlateGray", 12079, 20303, 20303, 12079, 20303, 20303, 1
]).
page(1,"",1).
'
  def TgifObject.preamble
    PREAMBLE
  end
  def initialize(color,x,y,width,height,asc,des)
    @color = color
    @x = x
    @y = y
    @width = width
    @height = height
    @asc = asc
    @des = des
    @offset = RelativeOffset.new(0,0)
    @size = Tgif.default_size
  end
  def width
    @width
  end
  def height
    @height
  end
  def set_color(color)
    @color = color
  end
  def set_height(height)
    @height = height
    self
  end
  def ascendant
    @asc
  end
  def descendant
    @des
  end
  def set_asc(asc)
    @asc = asc
    @des = @height-asc
    self
  end
  def set_pos(x,y)
    @x = x
    @y = y
    self
  end
  def move_rel(x,y)
    @x += x
    @y += y
    self
  end
  def offset(x,y)
    @offset.add(x,y)
    self
  end
  def absolute_offset(x,y)
    if @offset.kind_of?(RelativeOffset) then
      x0,y0 = @offset.instanciate(@width,@height)
      @offset = AbsoluteOffset.new(x0+x,y0+y)
    else
      @offset.add(x,y)
    end
    self
  end
  def set_size(size)
    new_w = @width.to_f*size/@size
    new_h = @height.to_f*size/@size
    new_a = @asc.to_f*size/@size
    @width = new_w.to_i
    @height = new_h.to_i
    @asc = new_a.to_i
    @des = @height-@asc
    @size = size
    self
  end
  def scriptsize
    set_size(size_script(@size))
  end
  def large
    set_size(size_large(@size))
  end
  def get_xoffset
    @offset.instanciate(@width,@height)[0]
  end
  def get_yoffset
    @offset.instanciate(@width,@height)[1]
  end
  def phys_x
    @x+get_xoffset
  end
  def phys_y
    @y+get_yoffset
  end
  def phys_xy
    off_x,off_y = @offset.instanciate(@width,@height)
    [@x+off_x,@y+@off_y]
  end
  def tgif_format(xoff=0,yoff=0)
    # subclassresponsibility
    ID[0] += 1
    res = ''
    if Tgif.debug_output then
      res << sprintf("box('red',%d,%d,%d,%d,0,1,1,%d,0,0,0,0,0,'1',[
]).
",phys_x+xoff,phys_y+yoff,phys_x+xoff+@width,phys_y+yoff+@height,ID[0])+
      sprintf("oval('blue',%d,%d,%d,%d,1,1,1,%d,0,0,0,0,0,'1',[
]).
",phys_x+xoff-2,phys_y+yoff-2,phys_x+xoff+2,phys_y+yoff+2,ID[0]+1)
      sprintf("poly('magenta',2,[\n\
	%d,%d,%d,%d],0,1,1,%d,0,0,0,0,10,4,0,0,0,'2','10','4',\n\
    \"0\",[\n]).
",phys_x+xoff,phys_y+yoff+@asc,phys_x+xoff+@width,phys_y+yoff+@asc,ID[0]+2)
      ID[0] += 3
    end
    res
  end
  def middle_down_box(xoff=0,yoff=0)
    res = sprintf("box('white',%d,%d,%d,%d,1,1,1,%d,0,0,0,0,0,'1',[\n]).\n",
        phys_x+xoff-1,phys_y+yoff-1,phys_x+xoff+@width+1,phys_y+yoff+@asc*2,ID[0])
    ID[0] += 1
    res
  end
  def _offset=(off)
    @offset = off
  end
  def clone
    x = super
    x._offset=@offset.clone
    x
  end
  def roman
  end
  def italic
  end
  def bold
  end
  def helvetica
  end
  def courier
  end
  def null  # do nothing
  end
end


class TgifTextObject < TgifObject
  def initialize(color,x,y,font,style,size,width,height,asc,des)
    super(color,x,y,width,height,asc,des)
    @font = font
    @style = style
    @size = size
  end
  def setfont(font,fontnum)
    if @font != 'Symbol' then
      @font = font
      @style = fontnum
    end
  end
  def roman
    setfont('Times-Roman',0)
  end
  def italic
    setfont('Times-Italic',2)
  end
  def bold
    setfont('Times-BoldItalic',3)
  end
  def helvetica
    setfont('Helvetica-Oblique',2)
  end
  def courier
    setfont('Courier',0)
  end
  def font
    @font
  end
end


class TextChar < TgifTextObject
  TEXT = {}
  def initialize(color,x,y,char,font,style,size,width,height,asc,des,is_rot,rot_info)
    super(color,x,y,font,style,size,width,height,asc,des)
    @char = char
    if rot_info.nil? then
      @rot_info = nil
    else
      @rot_info = rot_info[6..11]
    end
    @is_rot = is_rot
  end
  def tgif_format(xoff=0,yoff=0)
    just = 0
    color = 'black'
    x = phys_x+xoff
    y = phys_y+yoff
    rot_off_x = rot_off_y = 0
    unless @rot_info.nil? then
      if @rot_info[1] == 0 and @rot_info[2] == 0 then
        # not rotated, or upside down
        x += -@rot_info[4]+@width
        y += -@rot_info[5]+@height
        rbx = x+@width
        rby = y+@height
      elsif @rot_info[1] == 1000 and @rot_info[2] == -1000 then
        x += -@rot_info[4]+@width
        y += -@rot_info[5]
        rbx = x+@height
        rby = y+@width
      elsif @rot_info[1] == -1000 and @rot_info[2] == 1000 then
        x += -@rot_info[4]
        y += -@rot_info[5]+@height
        rbx = x+@height
        rby = y+@width
      end
    end
    res = super+
    sprintf("text('%s',%d,%d,'%s',%d,%d,1,%d,0,1,%d,%d,%d,",
           @color,x,y,
           @font,@style,@size,just,@width,@height,ID[0])+
    sprintf("0,%d,%d,0,0,0,0,0,2,0,0,0,0,\"\",0,%d,0,[",
           @asc,@des,@is_rot)
    unless @rot_info.nil? then
      res << sprintf("\n\t%d,%d,%d,%d,%d,%d,",x,y,x,y,rbx,rby)
      res << @rot_info.collect{|z|z.to_s}.join(',')
      res << sprintf(",%d,%d,%d,%d],[",x-1,y-1,rbx+1,rby+1)
    end
    res << sprintf("\n\t\"%s\"]).\n",@char)
    res
  end
  def TextChar.text(color,x,y,font,style,size,lines,just,rotate,pen,width,height,
     id,dummy1,asc,des,fill,v_space,rotation,locked,underline_on,underline,
     min_lbearing,max_rextra,double_byte,direction,
     custom_screen_font_name,compressed,is_rot,invisible,rot_info,textstring=nil)
    if textstring.nil? then
      textstring = rot_info
      rot_info = nil
    end
    TextChar.new(color,0,0,textstring[0],font,style,size,width,height,asc,des,is_rot,rot_info)
  end
  def TextChar.get(ch,size=Tgif.default_size,color=Tgif.default_color)
    if size.nil? then
      size = Tgif.default_size
    end
    if ch.size > 1 and ch =~ /^\d+$/ then # number
      return RomanSymbol.new(color,ch,size)
    end
    if TEXT[ch].nil? then
      return nil
    end
    c = TEXT[ch].clone
    if size != 30 then
      c.set_size(size)
    end
    c.set_color(color)
    c
  end
  def TextChar.init
    TEXT['A'] = text('black',64,48,'Times-Italic',2,30,1,1,0,1,18,35,18,0,28,7,0,0,0,0,0,2,-2,0,0,0,"",0,0,0,[
	"A"])
    TEXT['B'] = text('black',96,48,'Times-Italic',2,30,1,1,0,1,18,35,22,0,28,7,0,0,0,0,0,2,-1,0,0,0,"",0,0,0,[
	"B"])
    TEXT['C'] = text('black',128,48,'Times-Italic',2,30,1,1,0,1,20,35,24,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"C"])
    TEXT['D'] = text('black',160,48,'Times-Italic',2,30,1,1,0,1,22,35,26,0,28,7,0,0,0,0,0,2,-1,0,0,0,"",0,0,0,[
	"D"])
    TEXT['E'] = text('black',192,48,'Times-Italic',2,30,1,1,0,1,18,35,28,0,28,7,0,0,0,0,0,2,0,1,0,0,"",0,0,0,[
	"E"])
    TEXT['F'] = text('black',224,48,'Times-Italic',2,30,1,1,0,1,18,35,32,0,28,7,0,0,0,0,0,2,0,1,0,0,"",0,0,0,[
	"F"])
    TEXT['G'] = text('black',256,48,'Times-Italic',2,30,1,1,0,1,22,35,34,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"G"])
    TEXT['H'] = text('black',288,48,'Times-Italic',2,30,1,1,0,1,22,35,40,0,28,7,0,0,0,0,0,2,-1,0,0,0,"",0,0,0,[
	"H"])
    TEXT['I'] = text('black',320,48,'Times-Italic',2,30,1,1,0,1,10,35,42,0,28,7,0,0,0,0,0,2,-1,1,0,0,"",0,0,0,[
	"I"])
    TEXT['J'] = text('black',352,48,'Times-Italic',2,30,1,1,0,1,13,35,46,0,28,7,0,0,0,0,0,2,-1,1,0,0,"",0,0,0,[
	"J"])
    TEXT['K'] = text('black',384,48,'Times-Italic',2,30,1,1,0,1,20,35,48,0,28,7,0,0,0,0,0,2,0,2,0,0,"",0,0,0,[
	"K"])
    TEXT['L'] = text('black',416,48,'Times-Italic',2,30,1,1,0,1,17,35,50,0,28,7,0,0,0,0,0,2,-1,0,0,0,"",0,0,0,[
	"L"])
    TEXT['M'] = text('black',448,48,'Times-Italic',2,30,1,1,0,1,25,35,52,0,28,7,0,0,0,0,0,2,-1,0,0,0,"",0,0,0,[
	"M"])
    TEXT['N'] = text('black',480,48,'Times-Italic',2,30,1,1,0,1,20,35,54,0,28,7,0,0,0,0,0,2,-1,1,0,0,"",0,0,0,[
	"N"])
    TEXT['O'] = text('black',512,48,'Times-Italic',2,30,1,1,0,1,22,35,56,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"O"])
    TEXT['P'] = text('black',544,48,'Times-Italic',2,30,1,1,0,1,18,35,58,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"P"])
    TEXT['Q'] = text('black',576,48,'Times-Italic',2,30,1,1,0,1,22,35,60,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"Q"])
    TEXT['R'] = text('black',608,48,'Times-Italic',2,30,1,1,0,1,18,35,62,0,28,7,0,0,0,0,0,2,-1,0,0,0,"",0,0,0,[
	"R"])
    TEXT['S'] = text('black',640,48,'Times-Italic',2,30,1,1,0,1,15,35,64,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"S"])
    TEXT['T'] = text('black',672,48,'Times-Italic',2,30,1,1,0,1,17,35,66,0,28,7,0,0,0,0,0,2,0,1,0,0,"",0,0,0,[
	"T"])
    TEXT['U'] = text('black',704,48,'Times-Italic',2,30,1,1,0,1,22,35,68,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"U"])
    TEXT['V'] = text('black',736,48,'Times-Italic',2,30,1,1,0,1,18,35,70,0,28,7,0,0,0,0,0,2,0,1,0,0,"",0,0,0,[
	"V"])
    TEXT['W'] = text('black',768,48,'Times-Italic',2,30,1,1,0,1,25,35,72,0,28,7,0,0,0,0,0,2,0,1,0,0,"",0,0,0,[
	"W"])
    TEXT['X'] = text('black',800,48,'Times-Italic',2,30,1,1,0,1,18,35,74,0,28,7,0,0,0,0,0,2,-1,1,0,0,"",0,0,0,[
	"X"])
    TEXT['Y'] = text('black',832,48,'Times-Italic',2,30,1,1,0,1,17,35,76,0,28,7,0,0,0,0,0,2,0,1,0,0,"",0,0,0,[
	"Y"])
    TEXT['Z'] = text('black',864,48,'Times-Italic',2,30,1,1,0,1,17,35,78,0,28,7,0,0,0,0,0,2,0,1,0,0,"",0,0,0,[
	"Z"])
    TEXT['a'] = text('black',64,112,'Times-Italic',2,30,1,1,0,1,15,35,80,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"a"])
    TEXT['b'] = text('black',96,112,'Times-Italic',2,30,1,1,0,1,15,35,82,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"b"])
    TEXT['c'] = text('black',128,112,'Times-Italic',2,30,1,1,0,1,13,35,84,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"c"])
    TEXT['d'] = text('black',160,112,'Times-Italic',2,30,1,1,0,1,15,35,86,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"d"])
    TEXT['e'] = text('black',192,112,'Times-Italic',2,30,1,1,0,1,13,35,88,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"e"])
    TEXT['f'] = text('black',224,112,'Times-Italic',2,30,1,1,0,1,8,35,90,0,28,7,0,0,0,0,0,2,-4,5,0,0,"",0,0,0,[
	"f"])
    TEXT['g'] = text('black',256,112,'Times-Italic',2,30,1,1,0,1,15,35,92,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"g"])
    TEXT['h'] = text('black',288,112,'Times-Italic',2,30,1,1,0,1,15,35,94,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"h"])
    TEXT['i'] = text('black',320,112,'Times-Italic',2,30,1,1,0,1,8,35,96,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"i"])
    TEXT['j'] = text('black',352,112,'Times-Italic',2,30,1,1,0,1,8,35,98,0,28,7,0,0,0,0,0,2,-3,1,0,0,"",0,0,0,[
	"j"])
    TEXT['k'] = text('black',384,112,'Times-Italic',2,30,1,1,0,1,13,35,100,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"k"])
    TEXT['l'] = text('black',416,112,'Times-Italic',2,30,1,1,0,1,8,35,102,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"l"])
    TEXT['m'] = text('black',448,112,'Times-Italic',2,30,1,1,0,1,22,35,104,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"m"])
    TEXT['n'] = text('black',480,112,'Times-Italic',2,30,1,1,0,1,15,35,106,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"n"])
    TEXT['o'] = text('black',512,112,'Times-Italic',2,30,1,1,0,1,15,35,108,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"o"])
    TEXT['p'] = text('black',544,112,'Times-Italic',2,30,1,1,0,1,15,35,110,0,28,7,0,0,0,0,0,2,-2,0,0,0,"",0,0,0,[
	"p"])
    TEXT['q'] = text('black',576,112,'Times-Italic',2,30,1,1,0,1,15,35,112,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"q"])
    TEXT['r'] = text('black',608,112,'Times-Italic',2,30,1,1,0,1,12,35,114,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"r"])
    TEXT['s'] = text('black',640,112,'Times-Italic',2,30,1,1,0,1,12,35,116,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"s"])
    TEXT['t'] = text('black',672,112,'Times-Italic',2,30,1,1,0,1,8,35,118,0,28,7,0,0,0,0,0,2,0,1,0,0,"",0,0,0,[
	"t"])
    TEXT['u'] = text('black',704,112,'Times-Italic',2,30,1,1,0,1,15,35,120,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"u"])
    TEXT['v'] = text('black',736,112,'Times-Italic',2,30,1,1,0,1,13,35,122,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"v"])
    TEXT['w'] = text('black',768,112,'Times-Italic',2,30,1,1,0,1,20,35,124,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"w"])
    TEXT['x'] = text('black',800,112,'Times-Italic',2,30,1,1,0,1,13,35,126,0,28,7,0,0,0,0,0,2,-1,0,0,0,"",0,0,0,[
	"x"])
    TEXT['y'] = text('black',832,112,'Times-Italic',2,30,1,1,0,1,13,35,128,0,28,7,0,0,0,0,0,2,-1,0,0,0,"",0,0,0,[
	"y"])
    TEXT['z'] = text('black',864,112,'Times-Italic',2,30,1,1,0,1,12,35,130,0,28,7,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"z"])
    TEXT['Alpha'] = text('black',64,176,'Symbol',2,30,1,1,0,1,22,39,132,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"A"])
    TEXT['Beta'] = text('black',96,176,'Symbol',2,30,1,1,0,1,20,39,134,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"B"])
    TEXT['Gamma'] = text('black',128,176,'Symbol',2,30,1,1,0,1,18,39,136,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"G"])
    TEXT['Delta'] = text('black',160,176,'Symbol',2,30,1,1,0,1,18,39,138,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"D"])
    TEXT['Epsilon'] = text('black',192,176,'Symbol',2,30,1,1,0,1,18,39,140,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"E"])
    TEXT['Zeta'] = text('black',224,176,'Symbol',2,30,1,1,0,1,18,39,142,0,29,10,0,0,0,0,0,2,0,1,0,0,"",0,0,0,[
	"Z"])
    TEXT['Eta'] = text('black',256,176,'Symbol',2,30,1,1,0,1,22,39,144,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"H"])
    TEXT['Theta'] = text('black',288,176,'Symbol',2,30,1,1,0,1,22,39,148,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"Q"])
    TEXT['Iota'] = text('black',320,176,'Symbol',2,30,1,1,0,1,10,39,150,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"I"])
    TEXT['Kappa'] = text('black',352,176,'Symbol',2,30,1,1,0,1,22,39,154,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"K"])
    TEXT['Lambda'] = text('black',384,176,'Symbol',2,30,1,1,0,1,21,39,156,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"L"])
    TEXT['Mu'] = text('black',416,176,'Symbol',2,30,1,1,0,1,27,39,160,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"M"])
    TEXT['Nu'] = text('black',448,176,'Symbol',2,30,1,1,0,1,22,39,162,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"N"])
    TEXT['Xi'] = text('black',480,176,'Symbol',2,30,1,1,0,1,19,39,164,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"X"])
    TEXT['Omicron'] = text('black',512,176,'Symbol',2,30,1,1,0,1,22,39,166,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"O"])
    TEXT['Pi'] = text('black',544,176,'Symbol',2,30,1,1,0,1,23,39,168,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"P"])
    TEXT['Rho'] = text('black',576,176,'Symbol',2,30,1,1,0,1,17,39,170,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"R"])
    TEXT['Sigma'] = text('black',608,176,'Symbol',2,30,1,1,0,1,18,39,172,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"S"])
    TEXT['Tau'] = text('black',640,176,'Symbol',2,30,1,1,0,1,18,39,174,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"T"])
    TEXT['Upsilon'] = text('black',672,176,'Symbol',2,30,1,1,0,1,21,39,176,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"U"])
    TEXT['Phi'] = text('black',704,176,'Symbol',2,30,1,1,0,1,23,39,196,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"F"])
    TEXT['Chi'] = text('black',736,176,'Symbol',2,30,1,1,0,1,22,39,186,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"C"])
    TEXT['Psi'] = text('black',768,176,'Symbol',2,30,1,1,0,1,24,39,188,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"Y"])
    TEXT['Omega'] = text('black',800,176,'Symbol',2,30,1,1,0,1,23,39,190,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"W"])
    TEXT['alpha'] = text('black',64,240,'Symbol',2,30,1,1,0,1,19,39,198,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"a"])
    TEXT['beta'] = text('black',96,240,'Symbol',2,30,1,1,0,1,17,39,200,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"b"])
    TEXT['gamma'] = text('black',128,240,'Symbol',2,30,1,1,0,1,12,39,202,0,29,10,0,0,0,0,0,2,0,2,0,0,"",0,0,0,[
	"g"])
    TEXT['delta'] = text('black',160,240,'Symbol',2,30,1,1,0,1,15,39,204,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"d"])
    TEXT['epsilon'] = text('black',192,240,'Symbol',2,30,1,1,0,1,13,39,206,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"e"])
    TEXT['zeta'] = text('black',224,240,'Symbol',2,30,1,1,0,1,15,39,208,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"z"])
    TEXT['eta'] = text('black',256,240,'Symbol',2,30,1,1,0,1,18,39,212,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"h"])
    TEXT['theta'] = text('black',288,240,'Symbol',2,30,1,1,0,1,16,39,216,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"q"])
    TEXT['iota'] = text('black',320,240,'Symbol',2,30,1,1,0,1,10,39,220,0,29,10,0,0,0,0,0,2,-1,0,0,0,"",0,0,0,[
	"i"])
    TEXT['kappa'] = text('black',352,240,'Symbol',2,30,1,1,0,1,17,39,225,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"k"])
    TEXT['lambda'] = text('black',384,240,'Symbol',2,30,1,1,0,1,17,39,227,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"l"])
    TEXT['mu'] = text('black',416,240,'Symbol',2,30,1,1,0,1,17,39,229,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"m"])
    TEXT['nu'] = text('black',448,240,'Symbol',2,30,1,1,0,1,16,39,231,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"n"])
    TEXT['xi'] = text('black',480,240,'Symbol',2,30,1,1,0,1,15,39,233,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"x"])
    TEXT['omicron'] = text('black',512,240,'Symbol',2,30,1,1,0,1,17,39,235,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"o"])
    TEXT['pi'] = text('black',544,240,'Symbol',2,30,1,1,0,1,17,39,237,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"p"])
    TEXT['rho'] = text('black',576,240,'Symbol',2,30,1,1,0,1,17,39,239,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"r"])
    TEXT['sigma'] = text('black',608,240,'Symbol',2,30,1,1,0,1,18,39,241,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"s"])
    TEXT['tau'] = text('black',640,240,'Symbol',2,30,1,1,0,1,13,39,243,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"t"])
    TEXT['upsilon'] = text('black',672,240,'Symbol',2,30,1,1,0,1,17,39,245,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"u"])
    TEXT['phi'] = text('black',704,240,'Symbol',2,30,1,1,0,1,16,39,247,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"f"])
    TEXT['chi'] = text('black',736,240,'Symbol',2,30,1,1,0,1,17,39,249,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"c"])
    TEXT['psi'] = text('black',768,240,'Symbol',2,30,1,1,0,1,21,39,251,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"y"])
    TEXT['omega'] = text('black',800,240,'Symbol',2,30,1,1,0,1,21,39,253,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"w"])
    TEXT['0'] = text('black',64,304,'Times-Roman',0,30,1,1,0,1,15,32,257,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"0"])
    TEXT['1'] = text('black',96,304,'Times-Roman',0,30,1,1,0,1,15,32,259,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"1"])
    TEXT['2'] = text('black',128,304,'Times-Roman',0,30,1,1,0,1,15,32,261,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"2"])
    TEXT['3'] = text('black',160,304,'Times-Roman',0,30,1,1,0,1,15,32,263,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"3"])
    TEXT['4'] = text('black',192,304,'Times-Roman',0,30,1,1,0,1,15,32,265,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"4"])
    TEXT['5'] = text('black',224,304,'Times-Roman',0,30,1,1,0,1,15,32,267,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"5"])
    TEXT['6'] = text('black',256,304,'Times-Roman',0,30,1,1,0,1,15,32,269,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"6"])
    TEXT['7'] = text('black',288,304,'Times-Roman',0,30,1,1,0,1,15,32,271,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"7"])
    TEXT['8'] = text('black',320,304,'Times-Roman',0,30,1,1,0,1,15,32,273,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"8"])
    TEXT['9'] = text('black',352,304,'Times-Roman',0,30,1,1,0,1,15,32,275,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"9"])
    TEXT['!'] = text('black',64,368,'Times-Roman',0,30,1,1,0,1,10,32,277,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"!"])
    TEXT['@'] = text('black',96,368,'Times-Roman',0,30,1,1,0,1,28,32,279,0,26,6,0,0,0,0,0,2,-1,0,0,0,"",0,0,0,[
	"@"])
    TEXT['number'] = text('black',128,368,'Times-Roman',0,30,1,1,0,1,15,32,281,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"#"])
    TEXT['$'] = text('black',160,368,'Times-Roman',0,30,1,1,0,1,15,32,283,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"$"])
    TEXT['%'] = text('black',192,368,'Times-Roman',0,30,1,1,0,1,25,32,287,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"%"])
    TEXT['circumflex'] = text('black',224,368,'Times-Roman',0,30,1,1,0,1,14,32,289,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"^"])
    TEXT['&'] = text('black',256,368,'Times-Roman',0,30,1,1,0,1,23,32,291,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"&"])
    TEXT['amp'] = TEXT['&']
    TEXT['*'] = text('black',288,368,'Times-Roman',0,30,1,1,0,1,15,32,293,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"*"])
    TEXT['('] = text('black',320,368,'Times-Roman',0,30,1,1,0,1,10,32,297,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"("])
    TEXT[')'] = text('black',352,368,'Times-Roman',0,30,1,1,0,1,10,32,299,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	")"])
    TEXT['underscore'] = text('black',384,368,'Times-Roman',0,30,1,1,0,1,15,32,301,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"_"])
    TEXT['-'] = text('black',416,368,'Times-Roman',0,30,1,1,0,1,17,32,303,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"-"])
    TEXT['+'] = text('black',448,368,'Times-Roman',0,30,1,1,0,1,17,32,307,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"+"])
    TEXT['='] = text('black',480,368,'Times-Roman',0,30,1,1,0,1,17,32,311,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"="])
    TEXT['|'] = text('black',512,368,'Times-Roman',0,30,1,1,0,1,6,32,313,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"|"])
    TEXT['\\'] = text('black',544,368,'Times-Roman',0,30,1,1,0,1,8,32,315,0,26,6,0,0,0,0,0,2,-3,4,0,0,"",0,0,0,[
	"\\\\"])
    TEXT['sim'] = text('black',576,368,'Times-Roman',0,30,1,1,0,1,16,32,319,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"~"])
    TEXT['bquote'] = text('black',608,368,'Times-Roman',0,30,1,1,0,1,10,32,321,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"`"])
    TEXT['lbrace'] = text('black',64,432,'Times-Roman',0,30,1,1,0,1,14,32,325,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"{"])
    TEXT['rbrace'] = text('black',96,432,'Times-Roman',0,30,1,1,0,1,14,32,327,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"}"])
    TEXT['['] = text('black',128,432,'Times-Roman',0,30,1,1,0,1,10,32,329,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"["])
    TEXT[']'] = text('black',160,432,'Times-Roman',0,30,1,1,0,1,10,32,331,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"]"])
    TEXT[':'] = text('black',192,432,'Times-Roman',0,30,1,1,0,1,8,32,333,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	":"])
    TEXT[';'] = text('black',224,432,'Times-Roman',0,30,1,1,0,1,8,32,335,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	";"])
    TEXT['dquote'] = text('black',256,432,'Times-Roman',0,30,1,1,0,1,12,32,337,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"\""])
    TEXT['quote'] = text('black',288,432,'Times-Roman',0,30,1,1,0,1,10,32,339,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"'"])
    TEXT['<'] = text('black',320,432,'Times-Roman',0,30,1,1,0,1,17,32,341,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"<"])
    TEXT['>'] = text('black',352,432,'Times-Roman',0,30,1,1,0,1,17,32,343,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	">"])
    TEXT['gt'] = TEXT['>']
    TEXT['lt'] = TEXT['<']
    TEXT['.'] = text('black',384,432,'Times-Roman',0,30,1,1,0,1,8,32,345,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"."])
    TEXT[','] = text('black',416,432,'Times-Roman',0,30,1,1,0,1,8,32,347,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	","])
    TEXT['/'] = text('black',448,432,'Times-Roman',0,30,1,1,0,1,8,32,349,0,26,6,0,0,0,0,0,2,0,2,0,0,"",0,0,0,[
	"/"])
    TEXT['?'] = text('black',480,432,'Times-Roman',0,30,1,1,0,1,13,32,351,0,26,6,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"?"])
    TEXT['simeq'] = text('black',64,496,'Symbol',0,30,1,1,0,1,17,39,353,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"@"])
    TEXT['all'] = text('black',96,496,'Symbol',0,30,1,1,0,1,17,39,355,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"\""])
    TEXT['exist'] = text('black',96,496,'Symbol',0,30,1,1,0,1,17,39,355,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"$"])
    TEXT['bottom'] = text('black',128,496,'Symbol',0,30,1,1,0,1,20,39,357,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"^"])
    TEXT['therefore'] = text('black',160,496,'Symbol',0,30,1,1,0,1,26,39,359,0,29,10,0,0,0,0,0,2,0,0,0,0,"",0,0,0,[
	"\\\\"])
    TEXT['upperbar'] = text('black',192,496,'Symbol',0,30,1,1,0,1,15,39,361,0,29,10,0,0,0,0,0,2,0,18,0,0,"",0,0,0,[
	"`"])
## more special chars
    TEXT['Varupsilon'] = text('black',32,0,'Symbol',2,30,3,1,1,1,18,39,22,0,29,10,0,0,0,0,2,18,39,0,0,0,"",0,0,0,["\241"])
    TEXT['prime'] = text('black',32,35,'Symbol',2,30,1,1,1,1,7,39,24,0,29,10,0,0,0,0,2,7,39,0,0,0,"",0,0,0,["\242"])
    TEXT['leq'] = text('#000000',32,67,'Symbol',2,30,1,1,1,1,17,39,26,0,29,10,0,0,0,0,2,17,39,0,0,0,"",0,0,96,[
    	"\243"])
#    TEXT['slash'] = text('#000000',32,99,'Symbol',2,30,1,1,1,1,5,39,28,0,29,10,0,0,0,0,2,5,39,-2,3,0,"",0,0,128,[
#    	"\244"])
    TEXT['infty'] = text('#000000',32,13,'Symbol',2,30,1,1,1,1,20,39,30,0,29,10,0,0,0,0,2,20,39,0,0,0,"",0,0,160,[
    	"\245"])
#    TEXT['f'] = text('#000000',32,163,'Symbol',2,30,1,1,1,1,16,39,32,0,29,10,0,0,0,0,2,16,39,0,0,0,"",0,0,192,[
#    	"\246"])
    TEXT['club'] = text('#000000',32,195,'Symbol',2,30,1,1,1,1,22,39,34,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,224,[
    	"\247"])
    TEXT['diamond'] = text('#000000',32,227,'Symbol',2,30,1,1,1,1,22,39,36,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,256,[
    	"\250"])
    TEXT['heart'] = text('#000000',32,259,'Symbol',2,30,1,1,1,1,22,39,38,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,288,[
    	"\251"])
    TEXT['spade'] = text('#000000',32,29,'Symbol',2,30,1,1,1,1,22,39,40,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,320,[
    	"\252"])
    TEXT['leftrightarrow'] = text('#000000',32,323,'Symbol',2,30,1,1,1,1,31,39,42,0,29,10,0,0,0,0,2,31,39,0,0,0,"",0,0,352,[
    	"\253"])
    TEXT['leftarrow'] = text('#000000',32,355,'Symbol',2,30,1,1,1,1,30,39,44,0,29,10,0,0,0,0,2,30,39,0,0,0,"",0,0,384,[
    	"\254"])
    TEXT['gets'] = TEXT['leftarrow']
    TEXT['uparrow'] = text('#000000',32,387,'Symbol',2,30,1,1,1,1,17,39,51,0,29,10,0,0,0,0,2,17,39,0,0,0,"",0,0,416,[
    	"\255"])
    TEXT['rightarrow'] = text('#000000',32,419,'Symbol',2,30,1,1,1,1,30,39,53,0,29,10,0,0,0,0,2,30,39,0,0,0,"",0,0,448,[
    	"\256"])
    TEXT['to'] = TEXT['rightarrow']
    TEXT['downarrow'] = text('#000000',32,45,'Symbol',2,30,1,1,1,1,17,39,55,0,29,10,0,0,0,0,2,17,39,0,0,0,"",0,0,480,[
    	"\257"])
    TEXT['degree'] = text('#000000',96,3,'Symbol',2,30,1,1,1,1,11,39,57,0,29,10,0,0,0,0,2,11,39,0,0,0,"",0,0,32,[
    	"\260"])
    TEXT['pm'] = text('#000000',96,35,'Symbol',2,30,1,1,1,1,14,39,59,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,64,[
    	"\261"])
    TEXT['mp'] = text('#000000',96,35,'Symbol',2,30,1,1,1,1,14,39,59,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,1,64,[144,119,144,119,158,158,-1000,0,0,-1000,-32,20,143,118,159,159],[
    	"\261"]).set_asc(26)
    TEXT['pprime'] = text('#000000',96,67,'Symbol',2,30,1,1,1,1,12,39,61,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,96,[
    	"\262"])
    TEXT['geq'] = text('#000000',96,99,'Symbol',2,30,1,1,1,1,17,39,63,0,29,10,0,0,0,0,2,17,39,0,0,0,"",0,0,128,[
    	"\263"])
    TEXT['times'] = text('#000000',96,13,'Symbol',2,30,1,1,1,1,16,39,65,0,29,10,0,0,0,0,2,16,39,0,0,0,"",0,0,160,[
    	"\264"])
    TEXT['propto'] = text('#000000',96,163,'Symbol',2,30,1,1,1,1,19,39,67,0,29,10,0,0,0,0,2,19,39,0,0,0,"",0,0,192,[
    	"\265"])
    TEXT['partial'] = text('#000000',96,195,'Symbol',2,30,1,1,1,1,14,39,69,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,224,[
    	"\266"])
    TEXT['bullet'] = text('#000000',96,227,'Symbol',2,30,1,1,1,1,14,39,71,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,256,[
    	"\267"])
    TEXT['div'] = text('#000000',96,259,'Symbol',2,30,1,1,1,1,14,39,73,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,288,[
    	"\270"])
    TEXT['neq'] = text('#000000',96,29,'Symbol',2,30,1,1,1,1,17,39,75,0,29,10,0,0,0,0,2,17,39,0,0,0,"",0,0,320,[
    	"\271"])
    TEXT['equiv'] = text('#000000',96,323,'Symbol',2,30,1,1,1,1,16,39,77,0,29,10,0,0,0,0,2,16,39,0,0,0,"",0,0,352,[
    	"\272"])
    TEXT['approx'] = text('#000000',96,355,'Symbol',2,30,1,1,1,1,16,39,79,0,29,10,0,0,0,0,2,16,39,0,0,0,"",0,0,384,[
    	"\273"])
    TEXT['ldots'] = text('#000000',96,387,'Symbol',2,30,1,1,1,1,29,39,81,0,29,10,0,0,0,0,2,29,39,0,0,0,"",0,0,416,[
    	"\274"])
    TEXT['vert'] = text('#000000',96,419,'Symbol',2,30,1,1,1,1,17,39,83,0,29,10,0,0,0,0,2,17,39,0,0,0,"",0,0,448,[
    	"\275"])
    TEXT['dash'] = text('#000000',96,45,'Symbol',2,30,1,1,1,1,30,39,85,0,29,10,0,0,0,0,2,30,39,0,0,0,"",0,0,480,[
    	"\276"])
    TEXT['enter'] = text('#000000',160,3,'Symbol',2,30,1,1,1,1,19,39,89,0,29,10,0,0,0,0,2,19,39,0,0,0,"",0,0,32,[
    	"\277"])
    TEXT['aleph'] = text('#000000',160,35,'Symbol',2,30,1,1,1,1,17,39,91,0,29,10,0,0,0,0,2,17,39,0,0,0,"",0,0,64,[
    	"\300"])
    TEXT['Im'] = text('#000000',160,67,'Symbol',2,30,1,1,1,1,19,39,93,0,29,10,0,0,0,0,2,19,39,0,0,0,"",0,0,96,[
    	"\301"])
    TEXT['Re'] = text('#000000',160,99,'Symbol',2,30,1,1,1,1,23,39,95,0,29,10,0,0,0,0,2,23,39,0,0,0,"",0,0,128,[
    	"\302"])
    TEXT['calP'] = text('#000000',160,13,'Symbol',2,30,1,1,1,1,23,39,101,0,29,10,0,0,0,0,2,23,39,0,0,0,"",0,0,160,[
    	"\303"])
    TEXT['otimes'] = text('#000000',160,163,'Symbol',2,30,1,1,1,1,22,39,103,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,192,[
    	"\304"])
    TEXT['oplus'] = text('#000000',160,195,'Symbol',2,30,1,1,1,1,22,39,105,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,224,[
    	"\305"])
    TEXT['oslash'] = text('#000000',160,227,'Symbol',2,30,1,1,1,1,24,39,107,0,29,10,0,0,0,0,2,24,39,0,0,0,"",0,0,256,[
    	"\306"])
    TEXT['intersection'] = text('#000000',160,259,'Symbol',2,30,1,1,1,1,22,39,109,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,288,[
    	"\307"])
    TEXT['union'] = text('#000000',160,29,'Symbol',2,30,1,1,1,1,22,39,111,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,320,[
    	"\310"])
    TEXT['superset'] = text('#000000',160,323,'Symbol',2,30,1,1,1,1,20,39,113,0,29,10,0,0,0,0,2,20,39,0,0,0,"",0,0,352,[
    	"\311"])
    TEXT['superseteq'] = text('#000000',160,355,'Symbol',2,30,1,1,1,1,20,39,115,0,29,10,0,0,0,0,2,20,39,0,0,0,"",0,0,384,[
    	"\312"])
    TEXT['notsubset'] = text('#000000',160,387,'Symbol',2,30,1,1,1,1,20,39,117,0,29,10,0,0,0,0,2,20,39,0,0,0,"",0,0,416,[
    	"\313"])
    TEXT['subset'] = text('#000000',160,419,'Symbol',2,30,1,1,1,1,20,39,119,0,29,10,0,0,0,0,2,20,39,0,0,0,"",0,0,448,[
    	"\314"])
    TEXT['subseteq'] = text('#000000',160,45,'Symbol',2,30,1,1,1,1,20,39,121,0,29,10,0,0,0,0,2,20,39,0,0,0,"",0,0,480,[
    	"\315"])
    TEXT['in'] = text('#000000',224,3,'Symbol',2,30,1,1,1,1,19,39,123,0,29,10,0,0,0,0,2,19,39,0,0,0,"",0,0,32,[
    	"\316"])
    TEXT['notin'] = text('#000000',224,35,'Symbol',2,30,1,1,1,1,19,39,125,0,29,10,0,0,0,0,2,19,39,0,0,0,"",0,0,64,[
    	"\317"])
    TEXT['angle'] = text('#000000',224,67,'Symbol',2,30,1,1,1,1,23,39,127,0,29,10,0,0,0,0,2,23,39,0,0,0,"",0,0,96,[
    	"\320"])
    TEXT['nabla'] = text('#000000',224,99,'Symbol',2,30,1,1,1,1,22,39,129,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,128,[
    	"\321"])
    TEXT['registered'] = text('#000000',224,13,'Symbol',2,30,1,1,1,1,22,39,131,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,160,[
    	"\322"])
    TEXT['copyright'] = text('#000000',224,163,'Symbol',2,30,1,1,1,1,22,39,133,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,192,[
    	"\323"])
    TEXT['trademark'] = text('#000000',224,195,'Symbol',2,30,1,1,1,1,27,39,135,0,29,10,0,0,0,0,2,27,39,0,0,0,"",0,0,224,[
    	"\324"])
    TEXT['Prod'] = text('#000000',224,227,'Symbol',2,30,1,1,1,1,23,39,137,0,29,10,0,0,0,0,2,23,39,0,0,0,"",0,0,256,[
    	"\325"])
    TEXT['surd'] = text('#000000',224,259,'Symbol',2,30,1,1,1,1,17,39,139,0,29,10,0,0,0,0,2,17,39,0,0,0,"",0,0,288,[
    	"\326"])
    TEXT['cdot'] = text('#000000',224,29,'Symbol',2,30,1,1,1,1,7,39,141,0,29,10,0,0,0,0,2,7,39,0,0,0,"",0,0,320,[
    	"\327"])
    TEXT['not'] = text('#000000',224,323,'Symbol',2,30,1,1,1,1,20,39,143,0,29,10,0,0,0,0,2,20,39,0,0,0,"",0,0,352,[
    	"\330"])
    TEXT['and'] = text('#000000',224,355,'Symbol',2,30,1,1,1,1,18,39,145,0,29,10,0,0,0,0,2,18,39,0,0,0,"",0,0,384,[
    	"\331"])
    TEXT['or'] = text('#000000',224,387,'Symbol',2,30,1,1,1,1,18,39,147,0,29,10,0,0,0,0,2,18,39,0,0,0,"",0,0,416,[
    	"\332"])
    TEXT['Leftrightarrow'] = text('#000000',224,419,'Symbol',2,30,1,1,1,1,30,39,149,0,29,10,0,0,0,0,2,30,39,0,0,0,"",0,0,448,[
    	"\333"])
    TEXT['Leftarrow'] = text('#000000',224,45,'Symbol',2,30,1,1,1,1,30,39,151,0,29,10,0,0,0,0,2,30,39,0,0,0,"",0,0,480,[
    	"\334"])
    TEXT['Uparrow'] = text('#000000',288,3,'Symbol',2,30,1,1,1,1,17,39,155,0,29,10,0,0,0,0,2,17,39,0,0,0,"",0,0,32,[
    	"\335"])
    TEXT['Rightarrow'] = text('#000000',288,35,'Symbol',2,30,1,1,1,1,30,39,157,0,29,10,0,0,0,0,2,30,39,0,0,0,"",0,0,64,[
    	"\336"])
    TEXT['Downarrow'] = text('#000000',288,67,'Symbol',2,30,1,1,1,1,17,39,159,0,29,10,0,0,0,0,2,17,39,0,0,0,"",0,0,96,[
    	"\337"])
    TEXT['whitediamond'] = text('#000000',288,99,'Symbol',2,30,1,1,1,1,14,39,161,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,128,[
    	"\340"])
    TEXT['langle'] = text('#000000',288,13,'Symbol',2,30,1,1,1,1,10,39,163,0,29,10,0,0,0,0,2,10,39,0,0,0,"",0,0,160,[
    	"\341"])
#    TEXT[''] = text('#000000',288,163,'Symbol',2,30,1,1,1,1,22,39,165,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,192,[
#    	"\342"])
#    TEXT[''] = text('#000000',288,195,'Symbol',2,30,1,1,1,1,22,39,167,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,224,[
#    	"\343"])
#    TEXT[''] = text('#000000',288,227,'Symbol',2,30,1,1,1,1,24,39,169,0,29,10,0,0,0,0,2,24,39,0,0,0,"",0,0,256,[
#    	"\344"])
    TEXT['Sum'] = text('#000000',288,259,'Symbol',2,30,1,1,1,1,22,39,171,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,288,[
    	"\345"])
    TEXT['lparenU'] = text('#000000',288,29,'Symbol',2,30,1,1,1,1,12,39,173,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,320,[
    	"\346"])
    TEXT['lparenM'] = text('#000000',288,323,'Symbol',2,30,1,1,1,1,12,39,175,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,352,[
    	"\347"])
    TEXT['lparenD'] = text('#000000',288,355,'Symbol',2,30,1,1,1,1,12,39,177,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,384,[
    	"\350"])
    TEXT['lbraU'] = text('#000000',288,387,'Symbol',2,30,1,1,1,1,12,39,181,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,416,[
    	"\351"])
    TEXT['lbraM'] = text('#000000',288,419,'Symbol',2,30,1,1,1,1,12,39,183,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,448,[
    	"\352"])
    TEXT['lbraD'] = text('#000000',288,45,'Symbol',2,30,1,1,1,1,12,39,185,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,480,[
    	"\353"])
    TEXT['lbraceU'] = text('#000000',352,3,'Symbol',2,30,1,1,1,1,14,39,189,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,32,[
    	"\354"])
    TEXT['lbraceM'] = text('#000000',352,35,'Symbol',2,30,1,1,1,1,14,39,191,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,64,[
    	"\355"])
    TEXT['lbraceD'] = text('#000000',352,67,'Symbol',2,30,1,1,1,1,14,39,193,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,96,[
    	"\356"])
#    TEXT[''] = text('#000000',352,99,'Symbol',2,30,1,1,1,1,14,39,195,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,128,[
#    	"\357"])
    TEXT['apple'] = text('#000000',352,13,'Symbol',2,30,1,1,1,1,22,39,197,0,29,10,0,0,0,0,2,22,39,0,0,0,"",0,0,160,[
    	"\360"])
    TEXT['rangle'] = text('#000000',352,163,'Symbol',2,30,1,1,1,1,10,39,199,0,29,10,0,0,0,0,2,10,39,0,0,0,"",0,0,192,[
    	"\361"])
    TEXT['smallint'] = text('#000000',352,195,'Symbol',2,30,1,1,1,1,10,39,201,0,29,10,0,0,0,0,2,10,39,0,0,0,"",0,0,224,[
    	"\362"])
    TEXT['bigintU'] = text('#000000',352,227,'Symbol',2,30,1,1,1,1,20,39,203,0,29,10,0,0,0,0,2,20,39,0,0,0,"",0,0,256,[
    	"\363"])
    TEXT['bigintM'] = text('#000000',352,259,'Symbol',2,30,1,1,1,1,20,39,205,0,29,10,0,0,0,0,2,20,39,0,0,0,"",0,0,288,[
    	"\364"])
    TEXT['bigintD'] = text('#000000',352,29,'Symbol',2,30,1,1,1,1,20,39,212,0,29,10,0,0,0,0,2,20,39,0,0,0,"",0,0,320,[
    	"\365"])
    TEXT['rparenU'] = text('#000000',352,323,'Symbol',2,30,1,1,1,1,12,39,214,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,352,[
    	"\366"])
    TEXT['rparenM'] = text('#000000',352,355,'Symbol',2,30,1,1,1,1,12,39,216,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,384,[
    	"\367"])
    TEXT['rparenD'] = text('#000000',352,387,'Symbol',2,30,1,1,1,1,12,39,219,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,416,[
    	"\370"])
    TEXT['rbraU'] = text('#000000',352,419,'Symbol',2,30,1,1,1,1,12,39,221,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,448,[
    	"\371"])
    TEXT['rbraM'] = text('#000000',352,45,'Symbol',2,30,1,1,1,1,12,39,223,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,480,[
    	"\372"])
    TEXT['rbraD'] = text('#000000',416,3,'Symbol',2,30,1,1,1,1,12,39,227,0,29,10,0,0,0,0,2,12,39,0,0,0,"",0,0,32,[
    	"\373"])
    TEXT['rbraceU'] = text('#000000',416,35,'Symbol',2,30,1,1,1,1,14,39,229,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,64,[
    	"\374"])
    TEXT['rbraceM'] = text('#000000',416,67,'Symbol',2,30,1,1,1,1,14,39,231,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,96,[
    	"\375"])
    TEXT['rbraceD'] = text('#000000',416,99,'Symbol',2,30,1,1,1,1,14,39,233,0,29,10,0,0,0,0,2,14,39,0,0,0,"",0,0,128,[
    	"\376"])
#
    TEXT['ni'] =      text('#000000',147,11,'Symbol',2,30,1,1,1,1,19,39,234,0,29,10,0,0,0,0,2,19,39,0,0,0,"",0,1,0,[147,121,147,121,166,160,-1000,0,0,-1000,-31,21,146,120,167,161],[
	"\316"]).set_asc(26)
##
    TEXT['~'] = TgifObject.new('white',0,0,Tgif.default_size/2,Tgif.default_size,Tgif.default_size,0)
    TEXT['halfspace'] = TgifObject.new('white',0,0,Tgif.default_size/4,Tgif.default_size,Tgif.default_size,0)
    TEXT['quadspace'] = TgifObject.new('white',0,0,Tgif.default_size/8,Tgif.default_size,Tgif.default_size,0)
    TEXT['null'] = TgifObject.new('white',0,0,0,0,0,0)
    TEXT['log'] = RomanSymbol.new('black','log')
    TEXT['exp'] = RomanSymbol.new('black','exp')
    TEXT['sin'] = RomanSymbol.new('black','sin')
    TEXT['cos'] = RomanSymbol.new('black','cos')
    TEXT['tan'] = RomanSymbol.new('black','tan')
    TEXT['erf'] = RomanSymbol.new('black','erf')
    TEXT['if'] = RomanSymbol.new('black','if')
    TEXT['then'] = RomanSymbol.new('black','then')
    TEXT['else'] = RomanSymbol.new('black','else')
    TEXT['otherwise'] = RomanSymbol.new('black','otherwise')
    TEXT['iff'] = RomanSymbol.new('black','iff')
#    TEXT['mp'] = CompositeChar.new('black',30,'+','-',0,0.05,0,-0.2)
#    TEXT['to'] = TgifHArrow.new('black',0,0,20).set_height(12).set_asc(12)
#    TEXT['gets'] = TgifHArrow.new('black',0,0,20,TgifHArrow::BEGIN_ARROW).set_height(12).set_asc(12)
    TEXT['integral'] = Polygon.new('black',27,[
	111,66,109,66,107,69,103,85,99,108,96,129,95,138,93,147,
	91,150,89,151,90,148,88,147,85,148,85,150,88,152,93,150,
	97,138,103,108,105,88,109,65,111,67,110,69,112,70,113,70,
	114,69,114,68,111,66],1,1,1,1,50,0,0,0,0,0,'1',
    "ffffffe",[]).set_asc(55)
    TEXT['biglbrace'] = Polygon.new('black',17,[
	204,48,159,80,159,288,144,312,136,320,144,328,159,352,159,560,
	204,592,174,544,174,352,144,324,136,320,144,316,174,288,174,96,
	204,48],1,1,1,1,2,0,0,0,0,0,'1',
    "ffff8",[]).set_width(10)
    TEXT['bigrbrace'] = Polygon.new('black',17,[
	240,48,285,80,285,288,300,312,308,320,300,328,285,352,285,560,
	240,592,270,544,270,352,300,324,308,320,300,316,270,288,270,96,
	240,48],1,1,1,1,30,0,0,0,0,0,'1',
    "ffff8",[]).set_width(10)
    TEXT['biglparen'] = Polygon.new('black',13,[
	192,64,184,72,144,144,128,320,144,496,184,568,192,576,176,544,
	160,496,144,320,160,144,176,96,192,64],1,1,1,1,0,0,0,0,0,0,'1',
    "fff8",[]).set_width(10)
    TEXT['bigrparen'] = Polygon.new('black',13,[
	280,64,288,72,328,144,344,320,328,496,288,568,280,576,296,544,
	312,496,328,320,312,144,296,96,280,64],1,1,1,1,10,0,0,0,0,0,'1',
    "fff8",[]).set_width(10)
    TEXT['biglbracket'] = Polygon.new('black',9,[
	256,64,192,64,192,576,256,576,256,572,204,572,204,68,256,68,
	256,64],1,1,1,0,2,0,0,0,0,0,'1',
    "000",[]).set_width(10)
    TEXT['bigrbracket'] = Polygon.new('black',9,[
	292,64,356,64,356,576,292,576,292,572,344,572,344,68,292,68,
	292,64],1,1,1,0,4,0,0,0,0,0,'1',
    "000",[]).set_width(10)
    TEXT['blackbox'] = Polygon.new('black',5,[
	128,64,128,128,144,128,144,64,128,64],1,1,1,0,80,0,0,0,0,0,'1',
    "00",[])
    TEXT['verticalbar'] = TEXT['blackbox'].clone.set_width(1)
# originally 'heart'
    TEXT['whiteheart'] = Polygon.new('black',13,[
	192,128,188,120,160,96,128,104,128,160,188,212,192,216,196,212,
	256,160,256,104,224,96,196,120,192,128],0,1,1,1,0,0,0,0,0,0,'1',
    "fff8",[]).set_height(24).set_width(22).set_asc(22)
    TEXT['filledheart'] = TEXT['heart']
#    TEXT['filledheart'] = Polygon.new('black',13,[
#	192,128,188,120,160,96,128,104,128,160,188,212,192,216,196,212,
#	256,160,256,104,224,96,196,120,192,128],1,1,1,1,0,0,0,0,0,0,'1',
#    "fff8",[]).set_height(24).set_width(22).set_asc(22)
    TEXT['hbar'] = CompositeChar.new('black',30,'h','-',0,0,0.1,-0.2)
    nil
  end
end

class TgifSubString < TgifTextObject
  ESCAPE_CHAR = {
    "{" => "lbrace",
    "}" => "rbrace",
    "^" => "circumflex",
    "~" => "sim",
    "_" => "underscore",
    '"' => "dquote",
    "'" => "quote",
    "#" => "number",
  }
  def initialize(str,size,color)
    width = 0
    asc = 0
    des = 0
    for i in 0..str.size-1
      c = str[i,1]
      if c[0] < 0x80 then
        if ESCAPE_CHAR[c].nil? then 
  	  c_obj = TextChar.get(c,size,color) 
        else
	  c_obj = TextChar.get(ESCAPE_CHAR[c],size,color)
        end
        next if c_obj.nil?
        c_obj.roman
        width += c_obj.width
        asc = [asc,c_obj.ascendant].max
        des = [des,c_obj.descendant].max
      else
        width += size/2
        asc = [asc,size].max
      end
    end
    font = "Times-Roman"
    if str[0] > 0x80 then
      # EUC-JP
      font = "GothicBBB-Medium-EUC-H"
    end
    super(color,0,0,font,0,size,width,asc+des,asc,des)
    @str = str
  end
  def tgif_format(xoff=0,yoff=0)
    just = 0
    x = phys_x+xoff
    y = phys_y+yoff
    res = super+
    sprintf("text('%s',%d,%d,'%s',%d,%d,1,%d,0,1,%d,%d,%d,",
           @color,x,y,
           @font,@style,@size,just,@width,@height,ID[0])+
    sprintf("0,%d,%d,0,0,0,0,0,2,0,0,0,0,\"\",0,0,0,[",
           @asc,@des)
    res << sprintf("\n\t\"%s\"]).\n",@str)
    res
  end
end

class TgifObjectSequence < TgifObject
  def initialize(objs,size,color,x,y)
    @objs = objs
    width = 0
    asc = 0
    des = 0
    for o in @objs
      width += o.width
      asc = [asc,o.ascendant].max
      des = [des,o.descendant].max
    end
    super(color,x,y,width,asc+des,asc,des)
    @size = size
    set_pos(x,y)
  end
  def set_pos(x,y)
    w = 0
    for o in @objs
      o.set_pos(@x+w,@y)
      w += o.width
    end
    super
  end
  def do_all(proc,args)
    for o in @objs
      o.send(proc,*args)
    end
  end
  def move_rel(x,y); do_all(:move_rel, [x,y]); super; end
  def offset(x,y); do_all(:offset, [x,y]); super; end
  def set_size(size); do_all(:set_size, [size]); super; end
  def set_color(color); do_all(:set_color, [color]); super; end
  def tgif_format(xoff=0,yoff=0)
    res = ""
    for o in @objs
      res += o.tgif_format(xoff,yoff)
    end
    res
  end
end

class TgifString < TgifObjectSequence
  def initialize(str,size,color)
    objs = []
    i = j = 0
    flag = :us_ascii
    while i < str.size
      if flag == :kanji1 then
        # next byte of double-byte char
        flag = :kanji2
        i += 1
      elsif str[i] < 0x80 then
        if flag == :kanji2 then
          objs.push(TgifSubString.new(str[j,i-j],size,color))
          j = i
          i += 1
          flag = :us_ascii
        else
          i += 1
        end
      else
        if flag == :us_ascii and i > 0 then
          objs.push(TgifSubString.new(str[j,i-j],size,color))
          flag = :kanji1
          j = i
          i += 1
        elsif flag == :kanji1 then
          flag = :kanji2
          i += 1
        else
          flag = :kanji1
          i += 1
        end
      end
    end
    objs.push(TgifSubString.new(str[j,i-j],size,color))
    super(objs,size,color,0,0)
  end
end

class Polygon < TgifObject
  def initialize(color,npoint,points,fill,width,pen,curved,id, dash, rotation,
         locked, dummy,invisible, width_spec,smoothhinge,others)
    @points = []
    i = 0
    xmin = ymin = 9999999
    xmax = ymax = 0
    while i < npoint*2
      x = points[i]
      y = points[i+1]
      @points.push([x,y])
      xmin = x if x < xmin
      xmax = x if x > xmax
      ymin = y if y < ymin
      ymax = y if y > ymax
      i += 2
    end
    for p in @points
      p[0] -= xmin
      p[1] -= ymin
    end
    xdelta = xmax-xmin
    ydelta = ymax-ymin
    super(color,xmax,ymax,xdelta,ydelta,ydelta/2,ydelta/2)
    @size = 30
    @params1 = [fill,width,pen,curved]
    @params2 = [dash, rotation,locked, dummy,invisible]
    @width_spec = width_spec
    @smooth_hinge = smoothhinge
  end
  def set_params(points,params1,params2)
    @points = points
    @params1 = params1
    @params2 = params2
  end
  def clone
    n = super
    newpoints = []
    for p in @points
      newpoints.push(p.clone)
    end
    n.set_params(newpoints,@params1.clone,@params2.clone)
    n
  end
  def tgif_format(xoff=0,yoff=0)
    str = super
    str << sprintf("polygon('%s',%d,[",@color,@points.size)
    xo = phys_x+xoff
    yo = phys_y+yoff
    @points.each_index do |i|
      str << "\n\t" if i % 8 == 0
      str << (@points[i][0]+xo).to_i.to_s+","+(@points[i][1]+yo).to_i.to_s
      if i < @points.size-1 then
        str << ','
      end
    end
    str << "],"
    str << @params1.collect{|x| x.to_s}.join(',')
    str << ","+ID[0].to_s+","
    str << @params2.collect{|x| x.to_s}.join(',')
    str << sprintf(",'%s',\n\t\"%s\",[\n]).\n",@width_spec,@smooth_hinge)
    str
  end
  def set_size(size)
    mag = size.to_f/@size
    for p in @points
      p[0] = (p[0]*mag).to_i
      p[1] = (p[1]*mag).to_i
    end
    @size = size
    @width *= mag
    @height *= mag
    @asc *= mag
    @des *= mag
    self
  end
  def set_height(height)
    mag = height.to_f/@height
    for p in @points
      p[1] = (p[1]*mag).to_i
    end
    @height = height
    @asc *= mag
    @des *= mag
    self
  end
  def set_width(width)
    mag = width.to_f/@width
    for p in @points
      p[0] = (p[0]*mag).to_i
    end
    @width = width
    self
  end
end

class TgifLine < TgifObject
  def initialize(color,x1,y1,x2,y2,thickness=1)
    width = (x1-x2).abs
    height = (y1-y2).abs
    if x1 < x2 then
      x = x1
    else
      x = x2
    end
    if y1 < y2 then
      y = y1
    else
      y = y2
    end
    @thickness = thickness
    super(color,x,y,width,height,height,0)
  end
  def tgif_format(xoff=0,yoff=0)
    super+
    sprintf("poly('%s',2,[\n\
	%d,%d,%d,%d],0,%d,1,%d,0,0,0,0,10,4,0,0,0,'2','10','4',\n\
    \"0\",[\n]).\n",@color,phys_x+xoff,phys_y+yoff,phys_x+xoff+@width,phys_y+yoff+@height,@thickness,ID[0])
  end
end

class TgifHLine < TgifObject
  def initialize(color,x,y,width)
    super(color,x,y,width,2,1,1)
  end
  def tgif_format(xoff=0,yoff=0)
    super+
    sprintf("poly('%s',2,[\n\
	%d,%d,%d,%d],0,2,1,%d,0,0,0,0,10,4,0,0,0,'2','10','4',\n\
    \"0\",[\n]).\n",@color,phys_x+xoff,phys_y+yoff,phys_x+xoff+@width,phys_y+yoff,ID[0])
  end
  def scriptsize
    @width = scriptsize(@width)
  end
  def large
    @width = large(@width)
  end
end

class TgifHArrow < TgifObject
  NO_ARROW = 0
  END_ARROW = 1
  BEGIN_ARROW = 2
  BEGINEND_ARROW = 3
  def initialize(color,x,y,width,arrow=END_ARROW)
    super(color,x,y,width,3,1,2)
    @arrow = arrow
  end
  def tgif_format(xoff=0,yoff=0)
    super+
    sprintf("poly('%s',2,[\n\
	%d,%d,%d,%d],%d,1,1,%d,0,0,0,0,8,3,0,0,0,'1','8','3',\n\
    \"0\",[\n\
]).
",@color,phys_x+xoff,phys_y+yoff,phys_x+xoff+@width,phys_y+yoff,@arrow,ID[0])
  end
  def scriptsize
    @width = scriptsize(@width)
  end
  def large
    @width = large(@width)
  end
end

class TgifSimpleContainer < TgifObject
  def initialize(color,x,y,width,height,asc,des,content)
    @content = content
    super(color,x,y,width,height,asc,des)
  end
  def content_origin
    [@x,@y]
  end
  def scriptsize
    super
    @content.scriptsize
  end
  def large
    super
    @content.large
  end
  def set_size(size)
    super
    @content.set_size(size)
  end
  def set_pos(x,y)
    super
    @content.set_pos(*content_origin)
  end
  def move_rel(x,y)
    super
    @content.move_rel(x,y)
  end
  def set_content(content)
    @content = content
  end
  protected :set_content
  def clone
    n = super
    n.set_content(@content.clone)
    n
  end
end

class TgifSqrt < TgifSimpleContainer
  def initialize(color,x,y,content,size=Tgif.default_size)
    @size = size
    @flapsize = size*0.7
    super(color,x,y,content.width+@flapsize+2,content.height+2,
          content.ascendant+1,content.descendant+1,content)
    @content.set_pos(x+@flapsize+1,y+1)
  end
  def content_origin
    [@x+@flapsize+1,@y+1]
  end
  def tgif_format(xoff=0,yoff=0)
    x = phys_x+xoff
    y = phys_y+yoff
    x1 = x+@width-@content.width-1
    x3 = x+@flapsize/4
    x2 = x3+@flapsize/4
    y3 = y+@height-@flapsize/2
    y4 = y+@height-@flapsize/3
    thickidx = Math.log(@width.to_f*@height.to_f)
    thick = [((thickidx-2)/3).to_i,1].max
    res = super+
    @content.tgif_format(get_xoffset+xoff,get_yoffset+yoff)+
    sprintf("poly('%s',3,[\n\
	%d,%d,%d,%d,%d,%d],0,%d,1,%d,0,0,0,0,10,4,0,0,0,'2','10','4',\n\
    \"00\",[\n]).\n",@color,x+@width,y,x1,y,x2,y+@height, thick, ID[0])+
    sprintf("poly('%s',2,[\n\
	%d,%d,%d,%d],0,%d,1,%d,0,0,0,0,10,4,0,0,0,'2','10','4',\n\
    \"00\",[\n]).\n",@color,x2,y+@height,x3,y3,thick*2,ID[0]+1)+
    sprintf("poly('%s',2,[\n\
	%d,%d,%d,%d],0,%d,1,%d,0,0,0,0,10,4,0,0,0,'2','10','4',\n\
    \"00\",[\n]).\n",@color,x3,y3,x,y4,thick,ID[0]+2)
    ID[0] += 3
    res
  end
end

class TgifBar < TgifSimpleContainer
  def initialize(color,x,y,content,size=Tgif.default_size)
    @size = size
    @arrow = 0
    super(color,x,y,content.width+1,content.height+1,
          content.ascendant+1,content.descendant,content)
    @content.set_pos(x+1,y+1)
  end
  def content_origin
    [@x+1,@y+1]
  end
  def set_arrow(a)
    @arrow = a
  end
  def tgif_format(xoff=0,yoff=0)
    x = phys_x+xoff
    y = phys_y+yoff
    res = super+
    @content.tgif_format(get_xoffset+xoff,get_yoffset+yoff)+
    sprintf("poly('%s',2,[\n\
	%d,%d,%d,%d],%d,1,1,%d,0,0,0,0,10,4,0,0,0,'2','10','4',\n\
    \"00\",[\n]).\n",@color,x,y,x+@width,y,@arrow,ID[0])
    ID[0] += 3
    res
  end
end

class Expression < TgifObject
  PARAMS = {'intercharskip'=>2}
  def Expression.set_param(key,val)
    PARAMS[key] = val
  end
  # instance methods
  def initialize(color='black',width=0,height=0,asc=0)
    @chunk = []
    super(color,0,0,width,height,asc,height-asc)
    @rightmost = width
    @lowermost = height
    @skip = PARAMS['intercharskip']
  end
  def make_object(c)
    if c.kind_of?(String) then
      r = TextChar.get(c,@size,@color)
    else
      r = c
    end
    raise Error, "#{c.inspect}: no such character" if r.nil?
    r
  end
  def set_first_obj(r)
    r.set_pos(@x,@y)
    @width = r.width
    @height = r.height
    yf = r.get_yoffset
    @asc = r.ascendant-yf
    @des = r.descendant+yf
    @rightmost = @x+@width+@skip
    @lowermost = @y+@height+yf
  end
  def reset_asc(newasc)
    asc_diff = newasc-@asc
    for p in @chunk
      p[0].move_rel(0,asc_diff)
    end
    @asc = newasc
  end
  def <<(r)
    r = make_object(r)
    if @chunk.size == 0 then
      set_first_obj(r)
    else
      r.set_pos(@rightmost,@y)
      @rightmost += r.width+@skip
      @width += @skip+r.width
      newasc = r.ascendant-r.get_yoffset
      if newasc > @asc then
        reset_asc(newasc)
      end
      r.move_rel(0,@asc-r.ascendant)
      if r.descendant+r.get_yoffset > @des then
        @des = r.descendant+r.get_yoffset
      end
      @height = @asc+@des
      @lowermost = @y+@height
    end
    @chunk.push([r,:right])
  end
  def stick_top(r)
    r = make_object(r)
    r.set_pos(@rightmost,@y)
    if r.width > @width then
      @width = r.width
    end
    @chunk.push([r,:top])
  end
  def stick_bottom(r)
    r = make_object(r)
    r.set_pos(@rightmost,@y+@height-r.height)
    if r.width > @width then
      @width = r.width
    end
    @chunk.push([r,:bottom])
  end
  def stack(r)
    r = make_object(r)
    if @chunk.size == 0 then
      set_first_obj(r)
    else
      r.set_pos(@x,@lowermost)
      h = r.height+r.get_yoffset
      @lowermost += h
      @height += h
      @des += h
      if r.width > @width then
        @width = r.width
      end
    end
    @chunk.push([r,:down])
  end
  def stackup(r)
    r = make_object(r)
    if @chunk.size == 0 then
      set_first_obj(r)
    else 
      r.set_pos(@x,@y)
      h = r.height-r.get_yoffset
      if h > 0 then
        @height += h
        reset_asc(@asc+h)
        @lowermost += h
      end
      if r.width > @width then
        @width = r.width
      end
    end
    @chunk.push([r,:up])
  end
  def overlap(r)
    r = make_object(r)
    if @chunk.size == 0 then
      set_first_obj(r)
    else
      r.set_pos(@rightmost-r.width,@y)
    end
    @chunk.push([r,:overlap])
  end
  def move_rel(x,y)
    super
    for r in @chunk
      r[0].move_rel(x,y)
    end
    @rightmost = @x+@width
    @lowermost = @y+@height
    self
  end
  def set_pos(x,y)
    x_delta = x-@x
    y_delta = y-@y
    move_rel(x_delta,y_delta)
  end
  def offset(x,y)
    super
    for r in @chunk
      r[0].offset(x,y)
    end
    self
  end
  def tgif_format(xoff=0,yoff=0)
    res = super
    off_x,off_y = @offset.instanciate(@width,@height)
    my_xoff = off_x+xoff
    my_yoff = off_y+yoff
    for r in @chunk
      zz = r[0].tgif_format(my_xoff,my_yoff)
      res << zz
    end
    res
  end
  def rearrange_with_cmd(cmd,arg)
    return if @chunk.size == 0
    r = @chunk[0][0]
    if arg.nil? then
      r.send(cmd)
    else
      r.send(cmd,arg)
    end
    r.set_pos(@x,@y)
    @rightmost = @x+r.width+@skip
    @lowermost = @y+r.height
    @asc = r.ascendant
    @des = r.descendant
    for i in 1..@chunk.size-1
      r = @chunk[i]
      if arg.nil? then
        r[0].send(cmd)
      else
        r[0].send(cmd,arg)
      end
      case r[1]
      when :right
        r[0].set_pos(@rightmost,@y)
        if r[0].ascendant > @asc then
          reset_asc(r[0].ascendant)
        else
          r[0].move_rel(0,@asc-r[0].ascendant)
        end
        if r[0].descendant > @des then
          @des = r[0].descendant
        end
        @rightmost += r[0].width+@skip
      when :top
        r[0].set_pos(@rightmost,@y)
        @rightmost += r[0].width+@skip
      when :bottom
        r[0].set_pos(@rightmost,@y+@height-r[0].height)
        @rightmost += r[0].width+@skip
      when :up
        r[0].set_pos(@x,@y-r[0].height)
        @asc += r[0].height
      when :down
        r[0].set_pos(@x,@lowermost)
        @lowermost += r[0].height
        @des += r[0].height
      when :overlap
        r[0].set_pos(@rightmost-r[0].width,@y)
      end
    end
    @height = @asc+@des
    @width = @rightmost-@skip
    self
  end

  def scriptsize
    rearrange_with_cmd(:scriptsize,nil)
  end
  def large
    rearrange_with_cmd(:large,nil)
  end
  def set_size(size)
    rearrange_with_cmd(:set_size,size)
  end
  def set_color(col)
    rearrange_with_cmd(:set_color,col)
  end
  def change_font(fontcmd)
    for r in @chunk
      r[0].send(fontcmd)
    end
  end
  def roman
    change_font(:roman)
  end
  def bold
    change_font(:bold)
  end
  def italic
    change_font(:italic)
  end
  def helvetica
    change_font(:helvetica)
  end
  def courier
    change_font(:courier)
  end
  def change_skip(newskip)
    @skip = newskip
    rearrange_with_cmd(:null)
  end
  def push_chunk(x)
    @chunk.push(x)
  end
  def clear_chunk
    @chunk = []
  end
  protected :push_chunk,:clear_chunk
  def clone
    x = super
    x.clear_chunk
    for r in @chunk
      x.push_chunk([r[0].clone,r[1]])
    end
    x
  end
end

class RomanSymbol < Expression
  def initialize(color,str,size = Tgif.default_size)
    super(color)
    @skip = 0
    n = Expression.new
    str.each_byte do |c|
      x = TextChar.get(c.chr,size,color)
      next if x.nil?
      x.roman
      self << x
    end
  end
end    
class CompositeChar < TgifObject
  def initialize(color,size,char1,char2,offx1,offy1,offx2,offy2)
    @c1 = char1; @c2 = char2;
    @c1 = TextChar.get(char1,size,color) if char1.kind_of?(String)
    @c2 = TextChar.get(char2,size,color) if char2.kind_of?(String)
    if char1.nil? or char2.nil? then
      raise Error, "Bad char to composite: #{char1} #{char2}"
    end
    w = [@c1.width,@c2.width].max
    a = [@c1.ascendant,@c2.ascendant].max
    d = [@c1.descendant,@c2.descendant].max
    super(color,0,0,w,a+d,a,d)
    @c1.offset(offx1,offy1)
    @c2.offset(offx2,offy2)
  end
  def set_size(size)
    super
    @c1.set_size(size)
    @c2.set_size(size)
  end
  def tgif_format(xoff=0,yoff=0)
    x = get_xoffset
    y = get_yoffset
    super+@c1.tgif_format(x+xoff,y+yoff)+@c2.tgif_format(x+xoff,y+yoff)
  end
  def set_pos(x,y)
    super
    @c1.set_pos(x,y)
    @c2.set_pos(x,y)
  end
  def move_rel(x,y)
    super
    @c1.move_rel(x,y)
    @c2.move_rel(x,y)
  end
  def setchars(c1,c2)
    @c1 = c1
    @c2 = c2
  end
  protected :setchars
  def clone
    n = super
    n.setchars(@c1.clone,@c2.clone)
    n
  end
end

class TgifMatrix < TgifObject
  def initialize(nrow,ncol,size,just=:center)
    super('black',0,0,0,0,0,0)
    @nrow = nrow
    @ncol = ncol
    @rowskip = 2
    @colskip = size
    @just = just
    @size = size
    @matrix = Array.new(@nrow)
    @colwidth = Array.new(@ncol)
    @rowheight = Array.new(@nrow)
    for i in 0..@nrow-1
      @matrix[i] = Array.new(@ncol)
      @rowheight[i] = 0
    end
    for i in 0..@ncol-1
      @colwidth[i] = 0
    end
  end
  def fix_size
    for j in 0..@ncol
      @colwidth[j] = 0
    end
    for i in 0..@nrow-1
      @rowheight[i] = 0
      for j in 0..@ncol-1
        unless @matrix[i][j].nil? then
          if @matrix[i][j].width > @colwidth[j] then
            @colwidth[j] = @matrix[i][j].width
          end
          if @matrix[i][j].height > @rowheight[i] then
            @rowheight[i] = @matrix[i][j].height
          end
        end
      end
    end
    @width = -@colskip
    @height = -@rowskip
    for j in 0..@ncol-1
      @width += @colwidth[j]+@colskip
    end
    for i in 0..@nrow-1
      @height += @rowheight[i]+@rowskip
    end
    @asc = @height/2+@size/3
    @des = @height-@asc
  end
  def rearrange
    y = @y
    for i in 0..@nrow-1
      x = @x
      for j in 0..@ncol-1
        unless @matrix[i][j].nil? then
          p = @matrix[i][j]
          yd = y+(@rowheight[i]-p.height)/2
          case @just
          when :left
            xd = x
          when :center
            xd = x+(@colwidth[j]-p.width)/2
          when :right
            xd = x+@colwidth[j]-p.width
          end
          p.set_pos(xd,yd)
        end
        x += @colwidth[j]+@colskip
      end
      y += @rowheight[i]+@rowskip
    end
  end
  def set_size(size)
    super
    fix_size
    rearrange
  end
  def tgif_format(xoff=0,yoff=0)
    x = get_xoffset
    y = get_yoffset
    res = super
    for i in 0..@nrow-1
      for j in 0..@ncol-1
        next if @matrix[i][j].nil?
        res << @matrix[i][j].tgif_format(x+xoff,y+yoff)
      end
    end
    res
  end
  def set_pos(x,y)
    xdiff = x-@x
    ydiff = y-@y
    move_rel(xdiff,ydiff)
  end
  def move_rel(x,y)
    super
    for i in 0..@nrow-1
      for j in 0..@ncol-1
        next if @matrix[i][j].nil?
        @matrix[i][j].move_rel(x,y)
      end
    end
  end
  def []=(i,j,val)
    @matrix[i][j] = val
  end
  def clone
    n = super
    for i in 0..@nrow-1
      for j in 0..@ncol-1
        n[i,j]=@matrix[i][j].clone
      end
    end
    n
  end
end

class << Expression
  include Utils
  CMDS = {
    'def' => [3,:define_cmd],
    '_' => [1,:sub],
    '^' => [1,:sup],
    'color' => 2,
    'scriptsize' => 1,
    'large' => 1,
    'roman' => 1,
    'bold' => 1,
    'italic' => 1,
    'helvetica' => 1,
    'courier' => 1,
    'frac' =>2,
    'sum' => 2,
    'prod' => 2,
    'int' => 2,
    'stack' => 2,
    'stackup' => 2,
    'lstack' => 2,
    'lstackup' => 2,
    'rstack' => 2,
    'rstackup' => 2,
    'min' => 1,
    'max' => 1,
    'argmin' => 1,
    'argmax' => 1,
    'lim' => 1,
    'hat' => 1,
    'tilde' => 1,
    'vector' => 1,
    'sqrt' => 1,
    'bar' => 1,
    'upperarrow' => 1,
    'Lbrace' => [1,:leftbrace],
    'Rbrace' => [1,:rightbrace],
    'LRbrace' => [1,:leftrightbrace],
    'Lparen' => [1,:leftparen],
    'Rparen' => [1,:rightparen],
    'LRparen' => [1,:leftrightparen],
    'Lbra' => [1,:leftbracket],
    'Rbra' => [1,:rightbracket],
    'LRbra' => [1,:leftrightbracket],
    'Lbar' => [1,:leftbar],
    'Rbar' => [1,:rightbat],
    'LRbar' => [1,:leftrightbar],
  }
  KNOWN_COLOR = ['black','blue','magenta','red','green','cyan','yellow',
                 'white','pink','DarkSlateGray','CadetBlue']

  def from_token(tok,size=nil,color=nil, logger=STDERR)
    size ||= Tgif.default_size
    color ||= Tgif.default_color
    case tok
    when String
      if tok =~ /^".*"$/ then
        tok.sub!(/^"/,'')
        tok.sub!(/"$/,'')
        return TgifString.new(tok,size,color)
      else
        return TextChar.get(tok,size,color)
      end
    when TgifObject
      return tok
    end
    t = Expression.new
    i = 0
    prev = 0
    while i < tok.size
      c = tok[i]
      i += 1
      if CMDS.key?(c) then
        if CMDS[c].kind_of?(Array) then
          nargs,c = CMDS[c]
        else
          nargs = CMDS[c]
          c = c.intern
        end
        arg = []
        for j in 0..nargs-1
          arg.push(tok[i+j])
        end
        arg.push prev
        if c.kind_of?(Symbol) then
          z = send(c,arg,size,color)
          prev = c
        else
          z = interpret_macro(arg,c,size,color)
          prev = 0
        end
        i += nargs
      elsif c == 'matrix' then
        just = :center
        case tok[i]
        when 'left'
          just = :left; i += 1
        when 'right'
          just = :right; i += 1
        when 'center'
          just = :center; i += 1
        end
        unless tok[i] =~ /^\d+$/ or tok[i+1] =~ /^\d+$/ then
          raise Error, "matrix argument error: both #{tok[i]} and #{tok[i+1]} should be numbers"
        end
        nrow = tok[i].to_i
        ncol = tok[i+1].to_i
        z = TgifMatrix.new(nrow,ncol,size,just)
        nelem = nrow*ncol
        i += 2
        if tok.size-i < nelem then
          raise Error, "matrix argument error: too few argument: #{tok.size-i} should be #{nelem}"
        end
        for j in 0..nrow-1
          for k in 0..ncol-1
            z[j,k] = from_token(tok[i],size,color)
            i += 1
          end
        end
        z.fix_size
        z.rearrange
      else
        case c
        when TokenList
          z = Expression.from_token(c,size,color)
        when String
	  if c =~ /^".*"$/ then
            c.sub!(/^"/,'')
            c.sub!(/"$/,'')
            z = TgifString.new(c,size,color)
          else
            z = TextChar.get(c,size,color)
      if z.nil?
        c.split(//).each do |chr|
          _z = TextChar.get(chr, size, color)
          t << _z if _z
        end
      end
          end
        else
          raise Error, "tok[#{i}] is #{c.inspect}: not TokenList nor String\n"
        end
        prev = 0
      end
      if z.nil? then
        msg = "warning: Can't convert to Expression: #{c.inspect}"
        if logger.respond_to?(:warn)
          logger.warn(msg)
        else
          logger.puts(msg)
        end
      elsif z != :void
        t << z
      end
    end
    t
  end

  def interpret_macro(arg,body,size,color)
    from_token(body.apply_arg(arg),size,color)
  end
  def define_cmd(arg,size,color)
    unless arg[0].kind_of?(String) then
      raise Error, "Illegal def: macro name should be a single token"
    end
    macro = arg[0]
    unless arg[1].kind_of?(String) and arg[1] =~ /^\d+/ then
      raise Error, "Illegal def #{macro}: \# of argument should be a number"
    end
    narg = arg[1].to_i
    CMDS[macro] = [narg,arg[2]]
    :void
  end
  def color(arg,size,color)
    unless KNOWN_COLOR.include?(arg[0]) or arg[0] =~ /^#[0-9a-fA-F]+$/ then
      raise Error, "#{arg[0]}: unknown color"
    end
    from_token(arg[1],size,arg[0])
  end
  def scriptsize(arg,size,color)
    from_token(arg[0],size_script(size),color)
  end
  def large(arg,size,color)
    from_token(arg[0],size_large(size),color)
  end
  def fontcmd(arg,size,color,font)
    n = from_token(arg[0],size,color)
    n.send(font)
    n
  end
  def roman(arg,size,color)
    fontcmd(arg,size,color,:roman)
  end
  def bold(arg,size,color)
    fontcmd(arg,size,color,:bold)
  end
  def italic(arg,size,color)
    fontcmd(arg,size,color,:italic)
  end
  def helvetica(arg,size,color)
    fontcmd(arg,size,color,:helvetica)
  end
  def courier(arg,size,color)
    fontcmd(arg,size,color,:courier)
  end
  def sub(arg,size,color)
    c = from_token(arg[0],size_script(size),color)
    prev = arg[1]
    return nil if c.nil?
    x = -0.1*size
    if prev == :sup then
      x = -0.3*size
    end
    c.absolute_offset(x,0.1*size)
    c
  end
  def sup(arg,size,color)
    c = from_token(arg[0],size_script(size),color)
    prev = arg[1]
    return nil if c.nil? 
    x = 0
    if prev == :sub then
      x = -0.2*size
    end
    c.absolute_offset(x,-0.6*size)
    c
  end
  def frac(arg,size,color)
    n = Expression.new
    num = from_token(arg[0],size,color)
    den = from_token(arg[1],size,color)
    maxwidth = [num.width,den.width].max+4
    line = TgifHLine.new(color,0,0,maxwidth)
    num.absolute_offset((maxwidth-num.width)/2,0)
    den.absolute_offset((maxwidth-den.width)/2,0)
    n << num
    n.stack line
    n.stack den
    n.set_asc(num.height+size/4)
    n
  end
  def sum(arg,size,color)
    mathop('Sum',arg[0],arg[1],size,color)
  end
  def prod(arg,size,color)
    mathop('Prod',arg[0],arg[1],size,color)
  end
  def mathop(sym,inf,sup,size,color)
    sup = from_token(sup,size_script(size),color)
    inf = from_token(inf,size_script(size),color)
    sym = from_token(sym,size,color)
    sup.absolute_offset(0,sym.height*0.1)
    inf.absolute_offset(0,-sym.height*0.15)
#    sym.absolute_offset(0,sym.height*0.05)
    n = simplestack([sym,sup],[inf])
    n
  end
  def int(arg,size,color)
    sup = from_token(arg[1],size_script(size),color)
    inf = from_token(arg[0],size_script(size),color)
    int = TextChar.get('integral',size,color)
    n = Expression.new
    n << int
    nn = Expression.new(color,0,int.height,int.ascendant)
    nn.stick_top(sup)
    inf.absolute_offset(-int.width*0.4,0)
    nn.stick_bottom(inf)
    n << nn
    n
  end
  def simplestack(upobj,downobj,just=:center)
    n = Expression.new
    arg = upobj+downobj
    maxwidth = arg.collect{|x|x.width}.max
    for obj in upobj
      case just
      when :left
        xo = 0
      when :center
        xo = (maxwidth-obj.width)/2
      when :right
        xo = maxwidth-obj.width
      end
      obj.absolute_offset(xo,0)
      n.stackup obj
    end
    for obj in downobj
      case just
      when :left
        xo = 0
      when :center
        xo = (maxwidth-obj.width)/2
      when :right
        xo = maxwidth-obj.width
      end
      obj.absolute_offset(xo,0)
      n.stack obj
    end
    n
  end
  def do_stack(arg,size,color,just)
    arg.pop
    arg.each_index do |i|
      arg[i] = from_token(arg[i],size,color)
    end
    simplestack([],arg,just)
  end
  def do_stackup(arg,size,color,just)
    arg.pop
    arg.each_index do |i|
      arg[i] = from_token(arg[i],size,color)
    end
    simplestack(arg,[],just)
  end
  def stack(arg,size,color); do_stack(arg,size,color,:center); end
  def lstack(arg,size,color); do_stack(arg,size,color,:left); end
  def rstack(arg,size,color); do_stack(arg,size,color,:right); end
  def stackup(arg,size,color); do_stackup(arg,size,color,:center); end
  def lstackup(arg,size,color); do_stackup(arg,size,color,:left); end
  def rstackup(arg,size,color); do_stackup(arg,size,color,:right); end
  def mathop1(sym,arg,size,color)
    f = from_token(arg,size_script(size),color)
    f.offset(0,-0.2)
    simplestack([],[RomanSymbol.new(color,sym,size),f])
  end
  def min(arg,size,color); mathop1("min",arg[0],size,color); end
  def max(arg,size,color); mathop1("max",arg[0],size,color); end
  def argmin(arg,size,color); mathop1("argmin",arg[0],size,color); end
  def argmax(arg,size,color); mathop1("argmax",arg[0],size,color); end
  def lim(arg,size,color); mathop1("lim",arg[0],size,color); end
  def above(b,h,offset)
    n = Expression.new
    n << b
    h.absolute_offset(-(b.width-h.width)/2,h.height*offset)
    n.overlap(h)
    n
  end
  def hat(arg,size,color)
    b = from_token(arg[0],size,color)
    h = TextChar.get('circumflex',size_script(size),color)
    above(b,h,-0.4)
  end
  def tilde(arg,size,color)
    b = from_token(arg[0],size,color)
    h = TextChar.get('sim',size_script(size),color)
    above(b,h,-0.55)
  end
  def vector(arg,size,color)
    b = from_token(arg[0],size,color)
    h = TextChar.get('to',size_script(size),color)
    above(b,h,-0.3)
  end
  def sqrt(arg,size,color)
    b = from_token(arg[0],size,color)
    TgifSqrt.new(color,0,0,b,size)
  end
  def bar(arg,size,color)
    b = from_token(arg[0],size,color)
    TgifBar.new(color,0,0,b,size)
  end
  def upperarrow(arg,size,color)
    b = from_token(arg[0],size,color)
    n = TgifBar.new(color,0,0,b,size)
    n.set_arrow(TgifHArrow::END_ARROW)
    n
  end
  def do_left(arg,size,color,paren)
    n = Expression.new
    a = from_token(arg[0],size,color)
    b = TextChar.get(paren,nil,color)
    b.set_height(a.height)
    b.set_asc(a.ascendant)
    n << b
    n << a
    n.set_asc(n.height/2+size/4)
    n
  end
  def do_right(arg,size,color,paren)
    n = Expression.new
    a = from_token(arg[0],size)
    b = TextChar.get(paren,nil,color)
    b.set_height(a.height)
    b.set_asc(a.ascendant)
    n << a
    n << b
    n.set_asc(n.height/2+size/4)
    n
  end
  def do_leftright(arg,size,color,lparen,rparen)
    n = Expression.new
    a = from_token(arg[0],size,color)
    bl = TextChar.get(lparen,nil,color)
    br = TextChar.get(rparen,nil,color)
    bl.set_height(a.height)
    bl.set_asc(a.ascendant)
    br.set_height(a.height)
    br.set_asc(a.ascendant)
    n << bl
    n << a
    n << br
    n.set_asc(n.height/2+size/4)
    n
  end
  def leftbrace(arg,size,color); do_left(arg,size,color,'biglbrace'); end
  def rightbrace(arg,size,color); do_right(arg,size,color,'bigrbrace'); end
  def leftrightbrace(arg,size,color); do_leftright(arg,size,color,'biglbrace','bigrbrace'); end
  def leftparen(arg,size,color); do_left(arg,size,color,'biglparen'); end
  def rightparen(arg,size,color); do_right(arg,size,color,'bigrparen'); end
  def leftrightparen(arg,size,color); do_leftright(arg,size,color,'biglparen','bigrparen'); end
  def leftbracket(arg,size,color); do_left(arg,size,color,'biglbracket'); end
  def rightbracket(arg,size,color); do_right(arg,size,color,'bigrbracket'); end
  def leftrightbracket(arg,size,color); do_leftright(arg,size,color,'biglbracket','bigrbracket'); end
  def leftbar(arg,size,color); do_left(arg,size,color,'verticalbar'); end
  def rightbar(arg,size,color); do_right(arg,size,color,'verticalbar'); end
  def leftrightbar(arg,size,color); do_leftright(arg,size,color,'verticalbar','verticalbar'); end
end

  end
end
