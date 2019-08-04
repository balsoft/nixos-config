{ pkgs, config, lib, ... }:
{
  services.openvpn = {
    servers = {
      client = {
        config = ''
          proto tcp-client


          remote cz2.getstaticip.com 443 # non-stadard port for OpenVPN
          dev tun

          ignore-unknown-option ip-win32 dynamic 0
          ip-win32 dynamic 0

          nobind
          persist-key

          tls-client
          remote-cert-tls server
          #uncomment following line and comment verify-x509-name line if older OpenVPN version is installed on your device
          #tls-remote eu3.finevpn.com
          verify-x509-name eu3.finevpn.com name

          verb 3

          cipher AES-256-CBC
          auth SHA1
          pull

          auth-user-pass

          #if connection is terminated, it will attempt to connect without promting username and pass
          auth-retry nointeract

          redirect-gateway def1
          #dhcp-option DNS 8.8.8.8
          #dhcp-option DNS 8.8.4.4

          
          <ca>
          -----BEGIN CERTIFICATE-----
          MIIEyzCCA7OgAwIBAgIJAKPYkg7opAaCMA0GCSqGSIb3DQEBCwUAMIGaMQswCQYD
          VQQGEwJDWjEVMBMGA1UECBMMVXN0ZWNreSBLcmFqMRcwFQYDVQQHEw5Vc3RpIG5h
          ZCBMYWJlbTEUMBIGA1UEChMLRmluZVZQTi5jb20xCzAJBgNVBAsTAklUMRcwFQYD
          VQQDEw5GaW5lVlBOLmNvbSBDQTEfMB0GCSqGSIb3DQEJARYQaW5mb0BmaW5ldnBu
          LmNvbTAeFw0xODAyMjYxMjIzMzVaFw0yODAyMjQxMjIzMzVaMIGaMQswCQYDVQQG
          EwJDWjEVMBMGA1UECBMMVXN0ZWNreSBLcmFqMRcwFQYDVQQHEw5Vc3RpIG5hZCBM
          YWJlbTEUMBIGA1UEChMLRmluZVZQTi5jb20xCzAJBgNVBAsTAklUMRcwFQYDVQQD
          Ew5GaW5lVlBOLmNvbSBDQTEfMB0GCSqGSIb3DQEJARYQaW5mb0BmaW5ldnBuLmNv
          bTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALyWG6suxMedT1coO5Gc
          F/lTCARRK3vh6I5HWb9Rhbxq0fkxHn8vp3haHX2uJXf/udv7wR43Z+1p+nhdwGMD
          5rNUVf7AqSYAWprWI4GB5twuYQnh+Iqoj+T527WlYG0kEl47W0Yk/8EortcQtCg7
          yvM4+CF5LC9Kfy1rWGtcxWohYSw4KrDRbL8EVE7cJSGC/Mxphkfe3Vwop+Moa9ww
          b9USOocxHzI2hGq9M0hNtXUcMxwzhNVek+4JEKTm5cf28X0DIPvVOLRFbUBHAYXA
          L0/WqkZiW8A56h6Agwg8dgm7n9qsHY51A36Es5X05QVaL0XCJ7pVDQzDkkejTZpZ
          cl8CAwEAAaOCARAwggEMMB0GA1UdDgQWBBTb+GbEL1EZefoHABF/VZsw5UGy6zCB
          zwYDVR0jBIHHMIHEgBTb+GbEL1EZefoHABF/VZsw5UGy66GBoKSBnTCBmjELMAkG
          A1UEBhMCQ1oxFTATBgNVBAgTDFVzdGVja3kgS3JhajEXMBUGA1UEBxMOVXN0aSBu
          YWQgTGFiZW0xFDASBgNVBAoTC0ZpbmVWUE4uY29tMQswCQYDVQQLEwJJVDEXMBUG
          A1UEAxMORmluZVZQTi5jb20gQ0ExHzAdBgkqhkiG9w0BCQEWEGluZm9AZmluZXZw
          bi5jb22CCQCj2JIO6KQGgjAMBgNVHRMEBTADAQH/MAsGA1UdDwQEAwIBBjANBgkq
          hkiG9w0BAQsFAAOCAQEANZNy+OEKNoAFNo0TjBCCyaXL6Pr3rD++I8C2X6QtGY1E
          +754H3h4/vfSDMZFT8QvVrfOXnYrN+anjM5wGXLuYlECZ69zaINdkA5NCjoFSnhG
          EnArMcDeOfuCJJEIN9hsILvRdS+WW/UrMjF/minrACAAnwQInB0AVXinglzqvnYf
          WOogFS9WvQeNn+moWzEkpBipdpXn7flgrPQsU29kK8HEI3bek2YRJoCjhe5S3rGB
          73mS0NORGcpDwlUuQeU54Qtl9i/cs6PrbGia9AvrKcHipLJC9dTq1mMKvmEupDBm
          DtHIEdmbuqGBHTJvKERSSdPiqfwUP11hfXyCfoty5g==
          -----END CERTIFICATE-----
          </ca>

        '';
        up = ''
          ip route delete 0.0.0.0/1
          ip route delete 128.0.0.0/1

          ip route add table 42 default via 93.190.51.91
          ip route rule add from 93.190.51.91 table 42

        '';
        authUserPass.username = config.secrets.openvpn.user;
        authUserPass.password = config.secrets.openvpn.password;
      };
    };
  };
}
