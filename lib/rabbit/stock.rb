require 'rabbit/gtk'

require 'rabbit/rabbit'

module Rabbit
  module Stock
    include GetText

    RABBIT = "rabbit-rabbit"

    @@loaded = false

    module_function
    def init(logger)
      return if @@loaded
      @@loaded = true
      Gtk::Stock.add(RABBIT.to_sym, N_("Rabbit"))
      begin
        image_theme = Theme::Searcher.find_theme("rabbit-images", true)
        file = Theme::Searcher.find_file("lavie-icon.png", [image_theme])
        loader = ImageLoader.new(file)
        loader.resize(32, 32)
        factory = Gtk::IconFactory.new
        factory.add(RABBIT, Gtk::IconSet.new(loader.pixbuf))
        factory.add_default
      rescue LoadError
        logger.warn($!)
      end
    end
  end
end
