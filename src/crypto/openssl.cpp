#include <fc/crypto/openssl.hpp>

#include <fc/filesystem.hpp>

#include <boost/filesystem/path.hpp>

#include <cstdlib>
#include <string>
#include <stdlib.h>

namespace  fc 
{
    struct openssl_scope
    {
       static path _configurationFilePath;
       openssl_scope()
       {
          ERR_load_crypto_strings(); 
          OpenSSL_add_all_algorithms();

          const boost::filesystem::path& boostPath = _configurationFilePath;
          if(boostPath.empty() == false)
          {
            std::string varSetting("OPENSSL_CONF=");
            varSetting += _configurationFilePath.to_native_ansi_path();
#if defined(WIN32)
            _putenv((char*)varSetting.c_str());
#else
            putenv((char*)varSetting.c_str());
#endif
          }
#if OPENSSL_VERSION_NUMBER < 0x10100000L
         // no longer needed as of OpenSSL 1.1
         // if special initialization is necessary in versions 1.1 and above,
         // use OPENSSL_init_crypto
          OPENSSL_config(nullptr);
#endif
       }

       ~openssl_scope()
       {
#if not defined(LIBRESSL_VERSION_NUMBER)
          // No FIPS in LibreSSL.
          // https://marc.info/?l=openbsd-misc&m=139819485423701&w=2
          FIPS_mode_set(0);
#endif
          CONF_modules_unload(1);
          EVP_cleanup();
          CRYPTO_cleanup_all_ex_data();
          ERR_free_strings();
       }
    };

    path openssl_scope::_configurationFilePath;

    void store_configuration_path(const path& filePath)
    {
      openssl_scope::_configurationFilePath = filePath;
    }
   
    int init_openssl()
    {
      static openssl_scope ossl;
      return 0;
    }
}
