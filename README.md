# Hulk IRC Server Guide

## Installation

### From Hackage

    $ cabal install hulk

### From Github

    $ git clone git://github.com/chrisdone/hulk.git
    $ cd hulk
    $ cabal install

## Usage

### Configuration

    $ cp hulk.conf.example hulk.conf
    
You can edit the port, server name and MOTD file in here.
    
### Auth

    $ mkdir auth

Put a salt for your passwords in auth/passwd. For example:

    $ head -c 128 /dev/random | sha1sum > auth/passwd-key

Then generate a password for your IRC user:

    $ hulk-generate-pass -u demo -c=hulk.conf >> auth/passwd

(It will wait for a single line containing a pass and output the user
and sha1 sum.)

### Start the server

    $ hulk -chulk.conf

Logs / events will be outputted to stdout. This will be a
configuration option later. (Send me a patch if you already did this!)

Clients *must* connect with a password and user that matches the users
and passwords in your `auth/passwd` file.

## Using with SSL

You can use it with stunnel.

Change the port setting in hulk.conf:

    port = 6666

Generate an SSL certificate:

    $ openssl req -new -out hulk.pem -keyout hulk.pem -nodes -x509 -days 365

Make a stunnel.conf file:

    pid = /path/to/wherever/stunnel.pid
    client = no
    foreground = yes
    output = /dev/stdout
    cert = hulk.pem
    [hulk]
    connect = 127.0.0.1:6666
    accept = 6667
    
Then run it:
    
    stunnel stunnel.conf

(It may be in `/usr/sbin/stunnel` depending on your system.)

Then run hulk:

    hulk -chulk.conf

## Logging

Hulk doesn't support specific channel logging yet, but you can use a
logger bot.

    $ cabal install hog
    $ hog -h=127.0.0.1 --port=6666 -n=hog -u=hog --pass=mypassword --channels=#dev,#x -l/directory/of/logs -d5

`-d5` is the timeout before reconnect.

## Using an announcer bot

If you're using a private IRC server you're probably using it at a dev
company, and you probably want to make announcements about commits,
tickets, etc. from a feed.

You can use rss2irc. But you need a patched version which supports
sending the PASS command:

    $ git clone git://github.com/chrisdone/rss2irc.git
    $ cd rss2irc
    $ cabal install

Then run it:

    $ rss2irc http://myserver/foo.atom announce@127.0.0.1/#dev \
      -p 6667 -i 1 -l  --pass myannouncepass

## Utility for 'telling' people things externally

Sometimes you want automatic scripts to send notices to the IRC.

Save this script in `tell.sh`:

    (cat /mypath/tell-login; echo "NICK $1"; echo "TELL $2 :$3") | nc serverip 6667 -w 1 > /dev/null

Run it like this:

    $ tell announce '#dev' 'Stuff happened!'

And put this in your /mypath/tell-login:

    $ cat /opt/hulk/tell-login
    PASS mypassword
    USER myuser * * *
