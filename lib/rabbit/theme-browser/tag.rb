require 'yaml'

require 'gtk2'

require 'rabbit/rabbit'
require 'rabbit/utils'

module Rabbit
  class ThemeBrowser
    module Tag
      INFOS = {}
      
      module_function
      def update_info_by_yaml_file(filename, *args)
        filename = File.join(filename, *args)
        if File.exist?(filename)
          target = filename
        else
          target = Utils.find_path_in_load_path(filename)
        end
        raise NotExistError.new(filename) if target.nil?
        result = File.open(target) do |f|
          YAML.load(f.read)
        end
        INFOS.update(infos_pangoize(result))
      end

      def infos_pangoize(infos)
        result = {}
        infos.each_key do |name|
          props = {}
          result[name] = props
          (infos[name] || {}).each do |key, value|
            case key
            when "size"
              prop_value = value * Pango::SCALE
            when "weight"
              weight = "WEIGHT_#{value.upcase}"
              prop_value = Pango::FontDescription.const_get(weight)
            when "style"
              style = "STYLE_#{value.upcase}"
              prop_value = Pango::FontDescription.const_get(style)
            when "underline"
              underline = value.upcase
              prop_value = Pango::AttrUnderline.const_get(underline)
            else
              prop_value = value
            end
            props[key] = prop_value
          end
        end
        result
      end

      update_info_by_yaml_file("rabbit", "theme-browser", "tag.yaml")
    end
  end
end
