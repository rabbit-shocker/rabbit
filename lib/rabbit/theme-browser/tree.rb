require 'gtk2'

require 'rabbit/rabbit'

module Rabbit
  class ThemeBrowser
    class Tree
      include GetText
      
      MODEL = [
        [:name, String],
        [:title, String],
        [:type, String],
      ]
      
      attr_reader :view
      
      def initialize(page)
        @page = page
        init_gui
      end
      
      def select(name, type)
        name_column = column(:name)
        depth = type == "category" ? 1 : 2
        @view.model.each do |model, path, iter|
          if depth == path.depth and name == iter.get_value(name_column)
            @view.expand_to_path(path)
            @view.selection.select_iter(iter)
            break
          end
        end
      end
      
      private
      def column(key)
        MODEL.index(MODEL.assoc(key))
      end
      
      def init_gui
        @view = Gtk::TreeView.new
        model_types = MODEL.collect {|key, type| type}
        model = Gtk::TreeStore.new(*model_types)
        @view.set_model(model)
        init_columns
        init_model
      end
      
      def init_columns
        renderer = Gtk::CellRendererText.new
        params = {
          "text" => column(:title),
        }
        @view.insert_column(-1, _("Theme"), renderer, params)
        @view.selection.signal_connect("changed") do |selection|
          iter = selection.selected
          if iter
            name = iter.get_value(column(:name))
            type = iter.get_value(column(:type))
            @page.change_document(name, type)
          end
          false
        end
      end
      
      def init_model
        model = @view.model
        type = column(:type)
        name = column(:name)
        title = column(:title)
        categories = {}
        @page.themes.each do |entry|
          category = entry.category
          iter = categories[category]
          if iter.nil?
            iter = model.append(nil)
            categories[category] = iter
            iter[type] = "category"
            iter[name] = category
            iter[title] = _(category)
          end
          child_iter = model.append(iter)
          child_iter[type] = "theme"
          child_iter[name] = entry.name
          child_iter[title] = _(entry.title)
        end
        # @view.expand_all
      end
    end
  end
end
