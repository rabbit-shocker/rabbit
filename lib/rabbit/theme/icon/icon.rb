@icon_images ||= []
if @icon_image
  @icon_images.unshift(@icon_image)
end

canvas.icon_list = @icon_images.collect do |filename|
  ImageLoader.new(search_file(filename)).pixbuf
end
