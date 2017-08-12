# riker

Simple reverse proxy.

## Install

    $ stack install

## Running

Enable listening on port 80 on Linux:

    $ setcap 'cap_net_bind_service=+ep' /path/to/riker

Run like:

    $ riker --connect-host 127.0.0.1 --connect-port 10009 --listen-port 80

Done.
