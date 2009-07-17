module Rabbit
  module Element
    module Base
      module DrawHook
        @@draw_hook_names = []

        class << self
          def def_draw_hook(klass, name)
            @@draw_hook_names << name
            base_name = "#{name}_draw_proc"
            var_name = "@#{base_name}s"
            klass.module_eval(<<-EOC, __FILE__, __LINE__ + 1)
              def add_#{base_name}(name=nil, &block)
                #{var_name} << [block, name]
                block
              end

              def delete_#{base_name}(name=nil, &block)
                #{var_name}.reject! do |blk,|
                  blk == block
                end
              end

              def delete_#{base_name}_by_name(name)
                #{var_name}.reject! do |_, nm|
                  name === nm
                end
              end

              def clear_#{base_name}s
                #{var_name} = []
              end

              def #{base_name}s(name)
                #{var_name}.find_all do |_, nm|
                  name === nm
                end
              end

              def #{base_name}(name)
                #{var_name}.find do |_, nm|
                  name === nm
                end
              end
EOC
          end

          def def_draw_hooks(klass, *names)
            names.each do |name|
              klass.def_draw_hook(name)
            end
          end

          private
          def append_features(klass)
            super
            methods(false).each do |name|
              klass.module_eval(<<-EOC, __FILE__, __LINE__ + 1)
                def self.#{name}(*args)
                  #{self.name}.#{name}(self, *args)
                end
EOC
            end
          end
        end

        def clear_draw_procs
          @@draw_hook_names.each do |name|
            __send__("clear_#{name}_draw_procs")
          end
        end
      end
    end
  end
end
