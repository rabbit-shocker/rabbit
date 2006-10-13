def emphasize_keyword(target, keyword)
  to_container(target).substitute_text do |_, text|
    if /#{keyword}/u =~ text
      result = text.split(/(#{keyword})/u).collect do |sub_text|
        if sub_text == keyword
          Emphasis.new(Text.new(sub_text))
        else
          sub_text
        end
      end
      result
    else
      text
    end
  end
end
