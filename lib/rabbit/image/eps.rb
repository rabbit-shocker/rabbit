require "rabbit/utils"

require "rabbit/image/base"

module Rabbit
  module ImageManipulable
    class EPS < Base

      unshift_loader(self)
      
      GS_COMMANDS = %w(gs gswin32c)
      DEFAULT_DPI = 72
      
      include SystemRunner

      class << self
        def match?(filename)
          File.open(filename) do |f|
            f.each do |line|
              case line
              when /^%!PS-Adobe-\d+.\d+ EPS/i
                return true
              when /^%%/
                # ignore
              else
                return false
              end
            end
          end
          false
        rescue ArgumentError
          false
        end
      end
      
      private
      def ensure_resize(w, h)
        @pixbuf = load_image(w, h).pixbuf
      end

      def update_size
        load_image
      end

      def load_image(width=nil, height=nil)
        data = begin
                 eps_to_png(width, height)
               rescue EPSCanNotHandleError
                 eps_to_pnm(width, height)
               end
        load_by_pixbuf_loader(data)
      end
      
      def eps_to_png(width=nil, height=nil)
        eps_to(width, height, "pngalpha")
      end
      
      def eps_to_pnm(width=nil, height=nil)
        eps_to(width, height, "pnm")
      end
      
      def eps_to(width, height, device, *gs_options)
        x, y, w, h, r = eps_size
        width ||= w
        height ||= h
        resolution = r || DEFAULT_DPI
        res_x = (width.to_f / w * DEFAULT_DPI).round
        res_y = (height.to_f / h * DEFAULT_DPI).round
        
        adjust_eps_if_need(x, y) do |path|
          tmp = Tempfile.new("rabbit-image-eps")
          args = %W(-q -dBATCH -dNOPAUSE -sDEVICE=#{device}
            -sOutputFile=#{tmp.path} -dEPSFitPage
            -dGraphicsAlphaBits=4 -dTextAlphaBits=4
            -g#{width}x#{height} -r#{res_x}x#{res_y}
            #{path})
          if GS_COMMANDS.any? {|gs| run(gs, *args)}
            begin
              tmp.open
              tmp.binmode
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
        File.read(@filename).split(/(?:\r\n?|\n)/).each do |line|
          if /^%%BoundingBox:\s*/ =~ line
            next if $POSTMATCH.chomp == '(atend)'
            sx, sy, ex, ey = $POSTMATCH.scan(/-?\d+/).map{|x| Integer(x)}
            w, h = ex - sx, ey - sy
          elsif /^%%Feature:\s*\*Resolution\s*(\d+)dpi/ =~ line
            r = $1.to_i
          end
          break if r and sx and sy and w and h
        end
        [sx, sy, w, h, r]
      end

      def adjust_eps_if_need(x, y)
        if x and y and x > 0 and y > 0
          tmp = Tempfile.new("rabbit-loader-adjusted-eps")
          tmp.puts("#{x} neg #{y} neg translate")
          tmp.print(File.open(@filename) {|f| f.read})
          tmp.close
          yield tmp.path
        else
          yield @filename
        end
      end
    end
  end
end
