# datenverbrauch (data usage)

query data usage from the umts card in the hackspace


## usage

        ./datenverbrauch -h
        datenverbrauch - query internet access data usage

        Usage: datenverbrauch ((-v|--version) | (-u|--user <USER>) (-p|--pass <PASS>)
                              [--pub-quota <PUBLISH URL FOR QUOTA>]
                              [--pub-used <PUBLISH URL FOR USED>]
                              [--pub-available <PUBLISH URL FOR AVAILABLE>])
          run it to show the current usage, use '--pub-xxx' switch to publish the
          values. use $ts$ for the current timestamp in ms epoch and $value$ for the
          current value in the url.

        Available options:
          -h,--help                Show this help text
          -v,--version             app version
          -u,--user <USER>         provider login user
          -p,--pass <PASS>         provider login password
          --pub-quota <PUBLISH URL FOR QUOTA>
                                   endpoint for quota value
          --pub-used <PUBLISH URL FOR USED>
                                   endpoint for used value
          --pub-available <PUBLISH URL FOR AVAILABLE>
                                   endpoint for available value


currently the following holes are replaced in the '--pub-xxx' url:

    | Hole    | Replacement                         |
    |---------|-------------------------------------|
    |    $ts$ |actual timestamp in epoch millis     |
    | $value$ |the current value                    |


## example

  * query data usage

        ./datenverbrauch --user 0157..... --pass <PWD>
        Startup - date: 30.12.2015 18:16
        ------------------
        Quota:     5120 MB
        Used:      1166 MB
        Available: 3954 MB



  * query data usage and publish the quota and used values

        ./datenverbrauch --user 0157..... --pass <PWD> \
          --pub-quota 'http://example.com?ts=$ts$&type=quota&value=$value$' \
          --pub-used 'http://example.com?ts=$ts$&type=used&value=$value$'
        Startup - date: 30.12.2015 18:19
        Publish to: http://example.com?ts=1451495814368&type=quota&value=5120 - OK
        Publish to: http://example.com?ts=1451495814368&type=used&value=1166 - OK
        ------------------
        Quota:     5120 MB
        Used:      1166 MB
        Available: 3954 MB


  * query data usage und publish the available value

        datenverbrauch --user 0157..... --pass <PWD> \
          --pub-available 'http://example.com/available&value=$value$'
        Startup - date: 30.12.2015 18:20
        Publish to: http://example.com/available&value=3954 - ERROR: StatusCodeException ... 404 ...
        ------------------
        Quota:     5120 MB
        Used:      1166 MB
        Available: 3954 MB
          
