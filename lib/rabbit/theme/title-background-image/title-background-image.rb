@title_background_image_properties ||= {}
default_title_backgroud_image_properties = {
  "as_large_as_possible" => true,
  "align" => "center",
  "assign_box" => false,
  "keep_ratio" => true,
}

match(TitleSlide) do |slides|
  slides.each do |slide|
    image_properties = default_title_backgroud_image_properties.dup
    @title_background_image_properties.each do |key, value|
      value = value.to_s if value.is_a?(Symbol)
      image_properties[key.to_s.gsub(/-/, "_")] = value
    end
    background_image = slide["background-image"] || @title_background_image
    if background_image.nil?
      theme_exit("must specify 'background-image' slide property or " \
                 "@title_background_image!!")
    end
    properties = {
      :file_name => find_file(background_image),
      :proc_name => "title-background-image",
      :properties => image_properties,
    }
    apply_background_image_property(slide, properties)
  end
end
