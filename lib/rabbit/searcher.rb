require 'rabbit/utils'

Rabbit::Converter.keep_kcode("EUC-JP") do
  begin
    require 'migemo'
  rescue LoadError
  end
end

module Rabbit
  class Searcher
    @@migemo_static_dict = nil
    @@migemo_static_dict_cache = nil

    def initialize(canvas)
      @canvas = canvas
    end

    def regexp(text)
      unless text == @text
        @text = text
        @regexp = nil
      end
      @regexp ||= internal_regexp
    end

    private
    def internal_regexp
      if migemo_available?
        migemo_regexp
      else
        begin
          /#{@text}/iu
        rescue RegexpError
          /#{Regexp.escape(@text)}/iu
        end
      end
    end

    def migemo_regexp
      text = Converter.to_eucjp_from_utf8(@text)
      segments = migemo_split_text(text)
      if segments.size <= 1
        regexp_str = migemo_generate_regexp_str(text, false)
      else
        regexp_str1 = migemo_generate_regexp_str(text, true)
        regexp_str2 = segments.collect do |pattern|
          migemo_generate_regexp_str(pattern, true)
        end.join
        regexp_str = [regexp_str1, regexp_str2].join("|")
      end
      /#{Converter.to_utf8_from_eucjp(regexp_str)}/iu
    end

    def migemo_generate_regexp_str(pattern, with_paren)
      Converter.keep_kcode("EUC-JP") do
        migemo = Migemo.new(@@migemo_static_dict, pattern)
        migemo.dict_cache = @@migemo_static_dict_cache
        migemo.with_paren = with_paren
        migemo.regex
      end
    end

    def migemo_split_text(text)
      text.scan(/[A-Z]?[^A-Z]+|[A-Z]+/e)
    end

    def migemo_available?
      defined?(::Migemo) and have_migemo_static_dict?
    end

    def have_migemo_static_dict?
      if @@migemo_static_dict.nil?
        dict, dict_cache = search_migemo_static_dict
        @@migemo_static_dict, @@migemo_static_dict_cache = dict, dict_cache
      end
      not @@migemo_static_dict.nil?
    end

    def search_migemo_static_dict
      @canvas.migemo_dictionary_search_path.each do |target|
        if File.directory?(target)
          [
           File.join(target, @canvas.migemo_dictionary_name),
           File.join(target, "migemo", @canvas.migemo_dictionary_name),
          ].each do |guess|
            return make_migemo_dict(guess) if File.readable?(guess)
          end
        elsif File.readable?(target)
          return make_migemo_dict(target)
        end
      end
      nil
    end

    def make_migemo_dict(path)
      dict = MigemoStaticDict.new(path)
      dict_cache = nil
      dict_cache_path = "#{path}.cache"
      if File.readable?(dict_cache_path)
        dict_cache = MigemoDictCache.new(dict_cache_path)
      end
      [dict, dict_cache]
    end
  end
end
