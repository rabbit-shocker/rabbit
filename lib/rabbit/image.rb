require "tempfile"
require 'thread'

module Rabbit
  
  module ImageManipulable

    GS_COMMANDS = %w(gs gswin32c)
    DEFAULT_DPI = 72
    
    attr_accessor :keep_scale
    
    attr_reader :pixbuf
    
    def initialize(filename, *args, &block)
      unless File.exist?(filename)
        raise ImageFileDoesNotExistError.new(filename)
      end
      super(*args, &block)
      @filename = filename
      @keep_scale = true
      load_image
      @original_pixbuf = @pixbuf
    end
    
    def width
      @pixbuf.width
    end
    
    def height
      @pixbuf.height
    end
    
    def resize(w, h)
      if w.nil? and h.nil?
        return
      elsif @keep_scale
        wid = @original_pixbuf.width
        hei = @original_pixbuf.height
        if w and h.nil?
          h = (height * w.to_f / width).ceil
        elsif w.nil? and h
          w = (width * h.to_f / height).ceil
        end
      else
        w ||= width
        h ||= height
      end
      if w > 0 and h > 0 and [w, h] != [width, height]
        if eps?
          load_image(w, h)
        else
          @pixbuf = @original_pixbuf.scale(w, h)
        end
      end
    end
    alias_method :scale, :resize
    
    private
    def eps?
      /\.eps/i =~ File.extname(@filename)
    end
    
    def eps_to_png(width=nil, height=nil)
      x, y, w, h, r = eps_size
      width ||= w
      height ||= h
      resolution = r || DEFAULT_DPI
      res_x = (width.to_f / w * DEFAULT_DPI).round
      res_y = (height.to_f / h * DEFAULT_DPI).round

      adjust_eps_if_need(x, y) do |path|
        tmp = Tempfile.new("Rabbit")
        args = %W(-q -dBATCH -dNOPAUSE -sDEVICE=pngalpha
          -sOutputFile=#{tmp.path} -dEPSFitPage
          -dGraphicsAlphaBits=4 -dTextAlphaBits=4
          -g#{width}x#{height} -r#{res_x}x#{res_y}
          #{path})
        if GS_COMMANDS.any? {|gs| system(gs, *args)}
          begin
            tmp.open
            tmp.read
          ensure
            tmp.close
          end
        else
          raise EPSCanNotHandleError.new("gs #{args.join(' ')}", GS_COMMANDS)
        end
      end
    end

    def eps_size
      sx, sy, w, h, r = nil
      File.open(@filename) do |f|
        f.each do |line|
          if /^%%BoundingBox:\s*/ =~ line
            sx, sy, ex, ey = $POSTMATCH.scan(/\d+/).map{|x| Integer(x)}
            w, h = ex - sx, ey - sy
          elsif /^%%Feature:\s*\*Resolution\s*(\d+)dpi/ =~ line
            r = $1.to_i
          end
          break if r and sx and sy and w and h
        end
      end
      [sx, sy, w, h, r]
    end

    def adjust_eps_if_need(x, y)
      if x and y
        yield @filename
      else
        tmp = Tempfile.new("Rabbit")
        tmp.puts "#{x} neg #{y} neg translate"
        tmp.print File.open(@filename) {|f| f.read}
        tmp.close
        yield tmp.path
      end
    end
    
    def load_image(width=nil, height=nil)
      image = nil
      if eps?
        image = eps_to_png(width, height)
      else
        File.open(@filename) do |file|
          file.binmode
          image = file.read
        end
      end
      loader = Gdk::PixbufLoader.new
      loader.last_write(image)
      @pixbuf = loader.pixbuf
      resize(width, height)
    end
     
  end
  
  class ImageLoader

    include ImageManipulable

  end

end
