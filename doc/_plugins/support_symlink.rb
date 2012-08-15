module Jekyll
  class Site
    class File < ::File
      class << self
        def symlink?(path)
          false
        end
      end
    end
  end
end
