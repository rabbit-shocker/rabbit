if windows?
  match("**") do |elems|
    elems.prop_delete("style")
  end
end
