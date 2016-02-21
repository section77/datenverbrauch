# datenverbrauch (data usage)

query data usage for the umts card in the hackspace.


## birdview

  * login in provider webapp
  * find / parse balance
  * find / parse usage
  * publish values to endpoint (POST with values encoded in the URL)


## usage

        ./datenverbrauch -h
        datenverbrauch - query internet access data usage

        Usage: datenverbrauch ((-v|--version) | [-q|--quiet] (-u|--user <USER>)
                              (-p|--pass <PASS>) [--pub-quota <PUBLISH URL FOR QUOTA>]
                              [--pub-used <PUBLISH URL FOR USED>]
                              [--pub-available <PUBLISH URL FOR AVAILABLE>]
                              [--pub-balance <PUBLISH URL FOR BALANCE>]
                              [--available-warning ARG] [--available-critical ARG]
                              [--balance-warning ARG] [--balance-critical ARG])
          run it to show the current usage, use '--pub-xxx' switch to publish the
          values. use $ts$ for the current timestamp in ms epoch and $value$ for the
          current value in the url.

        Available options:
          -h,--help                Show this help text
          -v,--version             app version
          -q,--quiet               be quiet
          -u,--user <USER>         provider login user
          -p,--pass <PASS>         provider login password
          --pub-quota <PUBLISH URL FOR QUOTA>
                                   endpoint for quota value
          --pub-used <PUBLISH URL FOR USED>
                                   endpoint for used value
          --pub-available <PUBLISH URL FOR AVAILABLE>
                                   endpoint for available value
          --pub-balance <PUBLISH URL FOR BALANCE>
                                   endpoint for current balance
          --available-warning ARG  available warning threshold
          --available-critical ARG available critical threshold
          --balance-warning ARG    balance warning threshold
          --balance-critical ARG   balance critical threshold


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
        Balance:   15.0 €
        ------------------
        Quota:     5120 MB
        Used:      1166 MB
        Available: 3954 MB



  * query data usage and publish the quota and used values

        ./datenverbrauch --user 0157..... --pass <PWD> \
          --pub-quota 'http://httpbin.org/post?ts=$ts$&type=quota&value=$value$' \
          --pub-used 'http://httpbin.org/post?ts=$ts$&type=used&value=$value$'
        Startup - date: 30.12.2015 18:19
        Publish to: http://httpbin.org/post?ts=1451495814368&type=quota&value=5120 - OK
        Publish to: http://httpbin.org/post?ts=1451495814368&type=used&value=1166 - OK
        ------------------
        Balance:   15.0 €
        ------------------
        Quota:     5120 MB
        Used:      1166 MB
        Available: 3954 MB


  * query data usage und publish the available value

        datenverbrauch --user 0157..... --pass <PWD> \
          --pub-available 'http://httpbin.org/post?name=av&value=$value$'
        Startup - date: 30.12.2015 18:20
        Publish to: http://httpbin.org/post&name=av&value=3954 - Ok
        ------------------
        Balance:   15.0 €
        ------------------
        Quota:     5120 MB
        Used:      1166 MB
        Available: 3954 MB
          


## exit code


  * 0: all fine
  * 1: usage-available < (value of parameter --available-warning)
  * 1: balance-available < (value of parameter --balance-warning)
  * 2: usage-available < (value of parameter --available-critical)
  * 2: balance-available < (value of parameter --balance-critical)
  * 2: quota exhausted


## build / install

 * install [stack](https://www.stackage.org/) from [here](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)
 * stack install

this installs the application under '$HOME/.local/bin'

