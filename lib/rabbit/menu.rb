require "gtk2"

require "rabbit/gettext"
require "rabbit/theme"
require "rabbit/image"

module Rabbit

  class Menu

    include GetText

    file = Theme::Searcher.search_file("lavie_icon.png", ["rabbit"])
    loader = ImageLoader.new(file)
    loader.resize(16, 16)
    @@icon = loader.pixbuf
    
    def initialize(canvas)
      @canvas = canvas
      create_menu
    end

    def popup(button, time)
      @menu.popup(nil, nil, button, time)
    end
    
    private
    def create_menu
      ifp = Gtk::ItemFactory.new(Gtk::ItemFactory::TYPE_MENU, "<main>", nil)

      items = [
        [_("/Separator"), "<Tearoff>"],
        
        if @canvas.index_mode?
          [_("/Slide"), "<StockItem>", "",
           Gtk::Stock::DND, method(:toggle_index_mode)]
        else
          [_("/Index"), "<StockItem>", "",
           Gtk::Stock::INDEX, method(:toggle_index_mode)]
        end,

        [_("/Separator"), "<Separator>"],
        
        if @canvas.fullscreen?
          [_("/UnFullScreen"), "<StockItem>", "",
            Gtk::Stock::ZOOM_OUT, method(:toggle_fullscreen)]
        else
          [_("/FullScreen"), "<StockItem>", "",
            Gtk::Stock::ZOOM_FIT, method(:toggle_fullscreen)]
        end,
        
        [_("/Separator"), "<Separator>"],
        
        [_("/SaveAsImage"), "<StockItem>", "",
         Gtk::Stock::SAVE, method(:save_as_image)],

        if Renderer.printable?
          [_("/Print"), "<StockItem>", "",
            Gtk::Stock::PRINT, method(:print)]
        end,
        
        [_("/Separator"), "<Separator>"],
        
        # [_("/Jump"), "<StockItem>", "", Gtk::Stock::JUMP_TO],
        [_("/Jump")],
        
        [_("/Jump") + _("/Separator"), "<Tearoff>"],

        [_("/Separator"), "<Separator>"],
        
        [_("/Next"), "<StockItem>", "",
         Gtk::Stock::GO_FORWARD, method(:move_to_next)],
        [_("/Previous"), "<StockItem>", "",
         Gtk::Stock::GO_BACK, method(:move_to_previous)],
        [_("/First"), "<StockItem>", "",
         Gtk::Stock::GOTO_FIRST, method(:move_to_first)],
        [_("/Last"), "<StockItem>", "",
         Gtk::Stock::GOTO_LAST, method(:move_to_last)],
        
        [_("/Separator"), "<Separator>"],
        
        [_("/Iconify"), "<ImageItem>", "", @@icon, method(:iconify)],

        [_("/Separator"), "<Separator>"],
        
        [_("/ReloadTheme"), "<StockItem>", "",
          Gtk::Stock::REFRESH, method(:reload_theme)],

        [_("/Separator"), "<Separator>"],
        
        [_("/Quit"), "<StockItem>", "",
          Gtk::Stock::QUIT, method(:quit)],
      ]

      _move_to = method(:move_to)
      jump = _("/Jump") + "/"
      @canvas.pages.each_with_index do |page, i|
        items << ["#{jump}#{i}: #{page.title}", "<Item>", nil, nil, _move_to, i]
      end

      ifp.create_items(items.compact)

      @menu = ifp.get_widget("<main>")
    end

    def move_to_next(*args)
      @canvas.move_to_next_if_can
    end
    
    def move_to_previous(*args)
      @canvas.move_to_previous_if_can
    end
    
    def move_to_first(*args)
      @canvas.move_to_first
    end
    
    def move_to_last(*args)
      @canvas.move_to_last
    end
    
    def move_to(index, *args)
      @canvas.move_to_if_can(index)
    end

    def toggle_index_mode(*args)
      Thread.new do
        @canvas.toggle_index_mode
      end
    end
    
    def save_as_image(*args)
      Thread.new do
        @canvas.save_as_image
      end
    end

    def print(*args)
      Thread.new do
        @canvas.print
      end
    end

    def toggle_fullscreen(*args)
      @canvas.toggle_fullscreen
    end
    
    def iconify(*args)
      @canvas.iconify
    end
    
    def reload_theme(*args)
      @canvas.reload_theme
    end
    
    def quit(*args)
      @canvas.quit
    end

  end
  
end
