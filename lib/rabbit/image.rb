require "tempfile"
require 'thread'

module Rabbit
  
  module ImageManipulable

    GS_COMMANDS = %w(gs gswin32c)
    
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
    
    def eps_to_pnm(width=nil, height=nil)
      w, h = eps_size
      width ||= w
      height ||= h
      tmp = Tempfile.new("Rabbit")
      args = %W(-q -dBATCH -dNOPAUSE -sDEVICE=pnmraw
        -sOutputFile=#{tmp.path} -dEPSFitPage
        -dGraphicsAlphaBits=4 -dTextAlphaBits=4
        -g#{width}x#{height} #{@filename})
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

    def eps_size
      w, h = nil
      File.open(@filename) do |f|
        f.each do |line|
          if /^%%BoundingBox:\s*/ =~ line
            sx, sy, ex, ey = $POSTMATCH.scan(/\d+/).map{|x| Integer(x)}
            w, h = ex - sx, ey - sy
            break
          end
        end
      end
      [w, h]
    end

    def load_image(width=nil, height=nil)
      image = nil
      if eps?
        image = eps_to_pnm(width, height)
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
