require "optparse"

class OptionParser
  class Category
    def initialize(str)
      @name = str
    end

    def summarize(*args, &block)
      yield('')
      yield(@name)
    end

    def summarize_as_roff(&block)
      yield(".SH #{::OptionParser.roff_escape(@name)}")
    end
  end

  class Switch
    def summarize_as_roff(&block)
      var = nil
      opt_str = [@short, @long].flatten.collect {|s|
                  "\\fB#{::OptionParser.roff_escape(s)}\\fR"
                }.join(', ')
      opt_str << arg.sub(/\A([=\s\[]*)(.+?)([\s\]]*)\z/) {
        var = $2
        "#{$1}\\fI#{$2}\\fR#{$3}"
      } if arg
      yield('.TP')
      yield(opt_str)
      desc.each do |d|
        d_str = ::OptionParser.roff_escape(d)
        d_str.gsub!(var) { "\\fI#{var}\\fR" } if var
        yield(d_str)
      end
    end
  end

  class List
    def summarize_as_roff(&block)
      list.each do |opt|
        if opt.respond_to?(:summarize_as_roff)
          opt.summarize_as_roff(&block)
        end
        # FIXME: and otherwise process separators and banners...
      end
    end
  end

  # TODO: decide whether we show this option in the option summary.
  Officious['roff'] = proc do |parser|
    Switch::NoArgument.new do
      puts parser.roff
      exit
    end
  end

  # TODO: make this option user-visible after implementing all.
  Officious['man'] = proc do |parser|
    Switch::NoArgument.new do
      IO.popen('man -l -', 'w') do |io|
        io.puts parser.roff
      end
      exit
    end
  end

  def roff
    to = [%[.TH #{self.class.roff_escape(program_name.upcase)} "1"]]
    visit(:summarize_as_roff) do |l|
      to << l + $/
    end
    to
  end

  def category(str)
    top.append(Category.new(str), nil, nil)
  end

  def self.roff_escape(str)
    str.gsub(/[-\\]/, '\\\\\\&').gsub(/^[.']/, '\\&') # '
    # TODO: taken over from rd2man-lib.rb, necessary to be confirmed
  end
end
