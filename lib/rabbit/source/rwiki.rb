require 'rabbit/source/base'
require 'rwiki/soap/driver'

module Rabbit
  module Source
    class RWiki

      include Base
      include LimitAccessInterval
      
      def self.initial_args_description
        "RWIKI_SOAP_IF_URI and PAGE_NAME"
      end

      def initialize(encoding, logger, soap_if_uri, name)
        @soap_if_uri = soap_if_uri
        @name = name
        super(encoding, logger)
        @driver = ::RWiki::SOAP::Driver.new(@soap_if_uri)
        @driver.log_dir = tmp_dir_name
      end

      def need_read?
        super or old?(@last_modified, :modified)
      end
      
      private
      def _read
        @last_modified = modified
        @driver.src(@name)
      end

      def modified
        @driver.modified(@name)
      end

      def init_base
        set_base(::File.dirname(@soap_if_uri.to_s))
      end
    end

  end
end
