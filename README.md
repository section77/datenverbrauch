# datenverbrauch - query internet access data usage

for the internet access in our hackspace we use a umts card from 'alditalk'.

to keep a eye on our usage, we use `datenverbrauch` to extract the values from their website
and feed it per 'POST' request (values encoded in the URL) in a statistics webapp.


## usage

        [j@main ~]$ datenverbrauch -h
        datenverbrauch - query internet access data usage

        Usage: datenverbrauch ((-v|--version) | [-q|--quiet] (-u|--user <USER>)
                              (-p|--pass <PASS>) [--pub-quota <PUBLISH URL FOR QUOTA>]
                              [--pub-used <PUBLISH URL FOR USED>]
                              [--pub-available <PUBLISH URL FOR AVAILABLE>]
                              [--pub-balance <PUBLISH URL FOR BALANCE>]
                              [--pub-days-left <PUBLISH URL FOR PREPAID DAYS LEFT>]
                              [--available-warning ARG] [--available-critical ARG]
                              [--balance-warning ARG] [--balance-critical ARG]
                              [--provider-base-url PROVIDER_BASE_URL])
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
          --pub-days-left <PUBLISH URL FOR PREPAID DAYS LEFT>
                                   endpoint for prepaid days left
          --available-warning ARG  available warning threshold
          --available-critical ARG available critical threshold
          --balance-warning ARG    balance warning threshold
          --balance-critical ARG   balance critical threshold
          --provider-base-url PROVIDER_BASE_URL
                                   Base URL from the Provider Website to query the
                                   current values



currently the following holes are replaced in the '--pub-xxx' url:

    | Hole    | Replacement                         |
    |---------|-------------------------------------|
    |    $ts$ |actual timestamp in epoch millis     |
    | $value$ |the current value                    |


## example

  * query data usage and print it on stdout

        [j@main ~]$ datenverbrauch --user 0157xxxxxxxx --pass xxxxxxxxxxxxxx
        Startup - date: 09.01.2017 17:16 - version: 0.2.5-SNAPSHOT
        --------------------
        Balance:   20.13 €
        --------------------
        Quota:     5120 MB
        Used:       378 MB
        Available: 4742 MB
        --------------------
        Days left:      18
        --------------------





  * query data usage and publish the quota and used values

        [j@main ~]$ datenverbrauch --user 0157xxxxxxxx --pass xxxxxxxxxxxxxx \
          --pub-quota 'http://httpbin.org/post?ts=$ts$&type=quota&value=$value$' \
          --pub-used 'http://httpbin.org/post?ts=$ts$&type=used&value=$value$'
        Startup - date: 09.01.2017 17:19 - version: 0.2.5-SNAPSHOT
        --------------------
        Balance:   20.13 €
        --------------------
        Quota:     5120 MB
        Used:       378 MB
        Available: 4742 MB
        --------------------
        Days left:      18
        --------------------
        Publish to: http://httpbin.org/post?ts=1483978799624&type=quota&value=4742 - OK
        Publish to: http://httpbin.org/post?ts=1483978799624&type=used&value=378 - OK



## exit code

  * 0: all fine
  * 1: usage-available < (value of parameter --available-warning)
  * 1: balance-available < (value of parameter --balance-warning)
  * 2: usage-available < (value of parameter --available-critical)
  * 2: balance-available < (value of parameter --balance-critical)
  * 2: quota exhausted


## install

 * install [stack](http://haskellstack.org), the build tool
 * change in the project directory: `cd datenverbrauch`
 * build it: `stack install`

this installs the application under '$HOME/.local/bin'

