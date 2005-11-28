require 'rabbit/source/base'
require 'rwiki/soap/driver'

module Rabbit
  module Source
    class RWiki

      include Base
      include LimitAccessInterval
      
      def self.initial_args_description
        N_("[RWIKI_SOAP_IF_URI] and [PAGE_NAME]")
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
        begin
          @last_modified = modified
          @driver.src(@name)
        rescue ::SOAP::Error
          @logger.error($!.message)
          @last_modified = Time.now + MINIMUM_ACCESS_TIME
          ""
        end
      end

      def modified
        begin
          @driver.modified(@name)
        rescue ::SOAP::Error
          @logger.error($!.message)
          Time.now
        end
      end

      def init_base
        set_base(::File.dirname(@soap_if_uri.to_s))
      end
    end

  end
end
