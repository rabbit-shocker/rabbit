match("**") do |elements|
  have_preformatted_in_parent = Proc.new do |element|
    if element.nil?
      false
    elsif element.is_a?(PreformattedBlock)
      true
    else
      have_preformatted_in_parent.call(element.parent)
    end
  end

  have_preformatted_in_children = Proc.new do |element|
    if element.nil?
      false
    elsif element.is_a?(PreformattedBlock)
      true
    elsif element.respond_to?(:elements)
      element.elements.any? {|child| have_preformatted_in_children.call(child)}
    else
      false
    end
  end

  elements.each do |element|
    next if have_preformatted_in_parent.call(element)
    next if have_preformatted_in_children.call(element)
    element.substitute_newline
  end
end
