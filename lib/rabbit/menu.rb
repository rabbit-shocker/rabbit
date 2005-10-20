require "gtk2"

require "rabbit/gettext"
require "rabbit/utils"
require "rabbit/theme"
require "rabbit/image"

module Rabbit

  class Menu

    include GetText

    file = Theme::Searcher.search_file("lavie_icon.png", ["rabbit-icon"])
    loader = ImageLoader.new(file)
    loader.resize(16, 16)
    @@icon = loader.pixbuf
    
    def initialize(canvas)
      @canvas = canvas
      @menu = create_menu
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
            nil, method(:toggle_index_mode)]
        else
          [_("/Index"), "<StockItem>", "",
            Gtk::Stock::INDEX, method(:toggle_index_mode)]
        end,

        [_("/Separator"), "<Separator>"],

        if @canvas.fullscreen_available?
          if @canvas.fullscreen?
            [_("/UnFullScreen"), "<StockItem>", "",
              Gtk::Stock::ZOOM_OUT, method(:toggle_fullscreen)]
          else
            [_("/FullScreen"), "<StockItem>", "",
              Gtk::Stock::ZOOM_FIT, method(:toggle_fullscreen)]
          end
        else
          nil
        end,

        if @canvas.fullscreen_available?
          [_("/Separator"), "<Separator>"]
        else
          nil
        end,
        
        if @canvas.white_outing?
          [_("/UnWhiteOut"), "<StockItem>", "", nil, method(:toggle_white_out)]
        else
          [_("/WhiteOut"), "<StockItem>", "", nil, method(:toggle_white_out)]
        end,
        
        if @canvas.black_outing?
          [_("/UnBlackOut"), "<StockItem>", "", nil, method(:toggle_black_out)]
        else
          [_("/BlackOut"), "<StockItem>", "", nil, method(:toggle_black_out)]
        end,

        [_("/Separator"), "<Separator>"],
        
        if @canvas.comment_frame_available?
          if @canvas.showing_comment_frame?
            [_("/HideCommentFrame"), "<StockItem>", "",
              nil, method(:toggle_comment_frame)]
          else
            [_("/ShowCommentFrame"), "<StockItem>", "",
              Gtk::Stock::DIALOG_INFO, method(:toggle_comment_frame)]
          end
        else
          nil
        end,

        if @canvas.comment_view_available?
          if @canvas.showing_comment_view?
            [_("/HideCommentView"), "<StockItem>", "",
              nil, method(:toggle_comment_view)]
          else
            [_("/ShowCommentView"), "<StockItem>", "",
              nil, method(:toggle_comment_view)]
          end
        else
          nil
        end,

        if @canvas.comment_frame_available? or
            @canvas.comment_view_available?
          [_("/Separator"), "<Separator>"]
        else
          nil
        end,

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
        
        if @canvas.iconify_available?
          [_("/Iconify"), "<ImageItem>", "", @@icon, method(:iconify)]
        else
          nil
        end,
        
        if @canvas.iconify_available?
          [_("/Separator"), "<Separator>"]
        else
          nil
        end,
        
        [_("/Redraw"), "<StockItem>", "",
          Gtk::Stock::REFRESH, method(:redraw)],

        [_("/ReloadTheme"), "<StockItem>", "",
          Gtk::Stock::REFRESH, method(:reload_theme)],

        [_("/ChangeTheme")],
        [_("/ChangeTheme") + _("/Separator"), "<Tearoff>"],

        [_("/MergeTheme")],
        [_("/MergeTheme") + _("/Separator"), "<Tearoff>"],

        [_("/CacheAllSlides"), "<Item>", nil, nil, method(:cache_all_slides)],

        [_("/Separator"), "<Separator>"],
        
        [_("/SaveAsImage"), "<StockItem>", "",
         Gtk::Stock::SAVE, method(:save_as_image)],

        if Renderer.printable?
          [_("/Print"), "<StockItem>", "",
            Gtk::Stock::PRINT, method(:print)]
        end,
        
        [_("/Separator"), "<Separator>"],
        
        [_("/ResetAdjustment"), "<StockItem>", "",
          Gtk::Stock::CLEAR, method(:reset_adjustment)],
        
        [_("/Separator"), "<Separator>"],
        
        [_("/Quit"), "<StockItem>", "", Gtk::Stock::QUIT, method(:quit)],
      ]

      _move_to = method(:move_to)
      jump = _("/Jump") + "/"
      @canvas.slides.each_with_index do |slide, i|
        items << [
          "#{jump}#{i}: #{Utils.unescape_title(slide.title)}",
          "<Item>", nil, nil, _move_to, i
        ]
      end

      themes = Theme::Searcher.collect_theme
      
      _change_theme = method(:change_theme)
      change = _("/ChangeTheme") + "/"
      themes.each do |name|
        items << ["#{change}#{name}", "<Item>", nil, nil, _change_theme, name]
      end

      _merge_theme = method(:merge_theme)
      merge = _("/MergeTheme") + "/"
      themes.each do |name|
        items << ["#{merge}#{name}", "<Item>", nil, nil, _merge_theme, name]
      end

      ifp.create_items(items.compact)

      ifp.get_widget("<main>")
    end

    def create_menu_when_processing
      ifp = Gtk::ItemFactory.new(Gtk::ItemFactory::TYPE_MENU, "<main>", nil)

      items = [
        [_("/Separator"), "<Tearoff>"],
        
        [_("/Quit"), "<StockItem>", "",
          Gtk::Stock::QUIT, method(:confirm_quit)],
      ]

      ifp.create_items(items.compact)

      ifp.get_widget("<main>")
    end

    def toggle_white_out(*args)
      @canvas.toggle_white_out
    end
    
    def toggle_black_out(*args)
      @canvas.toggle_black_out
    end
    
    def toggle_comment_frame(*args)
      @canvas.toggle_comment_frame
    end

    def toggle_comment_view(*args)
      @canvas.toggle_comment_view
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
    
    def change_theme(name, *args)
      @canvas.apply_theme(name)
    end
    
    def merge_theme(name, *args)
      @canvas.merge_theme(name)
    end
    
    def redraw(*args)
      @canvas.redraw
    end
    
    def reload_theme(*args)
      @canvas.reload_theme
    end
    
    def quit(*args)
      @canvas.quit
    end

    def confirm_quit(*args)
      @canvas.confirm_quit
    end

    def cache_all_slides(*args)
      Thread.new do
        @canvas.cache_all_slides
      end
    end
    
    def reset_adjustment(*args)
      @canvas.reset_adjustment
    end
  end
end
