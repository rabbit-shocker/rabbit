require "forwardable"
require "gtk2"

require "rabbit/gettext"

module Rabbit

  class Menu

    extend Forwardable

    include GetText
    
    def_delegators(:@canvas, :pages)
    
    def initialize(canvas)
      @canvas = canvas
      create_menu
    end

    def popup(button, time)
      @menu.popup(nil, nil, button, time)
    end
    
    private
    def create_menu
      ifp = Gtk::ItemFactory.new(Gtk::ItemFactory::TYPE_MENU, "<main>")
      items = [
        [_("/_Separator"), "<Tearoff>"],
        [_("/_Next"), "<StockItem>", "",
          Gtk::Stock::GO_FORWARD, method(:move_to_next)],
        [_("/_Previous"), "<StockItem>", "",
          Gtk::Stock::GO_BACK, method(:move_to_previous)],
        [_("/_First"), "<StockItem>", "",
          Gtk::Stock::GOTO_FIRST, method(:move_to_first)],
        [_("/_Last"), "<StockItem>", "",
          Gtk::Stock::GOTO_LAST, method(:move_to_last)],
        [_("/_Separator"), "<Separator>"],
        [_("/_Jump")],
        [_("/_Jump") + _("/_Separator"), "<Tearoff>"],
      ]

      _move_to = method(:move_to)
      jump = _("/_Jump") + "/"
      pages.each_with_index do |page, i|
        items << ["#{jump}#{i}: #{page.title}", "<Item>", nil, nil, _move_to, i]
      end

      ifp.create_items(items)

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
    
  end
  
end
