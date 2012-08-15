module Jekyll
  class Site
    class File < ::File
      class << self
        def symlink?(path)
          super and !exist?(readlink(path))
        end
      end
    end
  end
end
