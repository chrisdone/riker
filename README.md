# riker [![Build Status](https://travis-ci.org/chrisdone/riker.svg)](https://travis-ci.org/chrisdone/riker)

Simple reverse proxy.

## Install

Binary releases for Linux and OS X are available [here](https://github.com/chrisdone/riker/releases).

Installing from source:

1. Get [stack](https://haskell-lang.org/get-started)
2. Run `stack install` in the repository directory.
3. Add `~/.local/bin/` to your `PATH`.

## Running

Enable listening on port 80 on Linux:

    $ setcap 'cap_net_bind_service=+ep' /path/to/riker

Run like:

    $ riker --connect-host 127.0.0.1 --connect-port 10009 --listen-port 80

Done.
