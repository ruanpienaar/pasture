#!/usr/bin/env python

# INSTALL:
# pip install tweepy
# pip install erlport

from __future__ import absolute_import, print_function

from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

from erlport.erlterms import Atom
from erlport.erlterms import List
from erlport.erlang import set_message_handler, cast

response_dest = ""

consumer_key="JmHsQPAzhAYTtsnDEblRkQtOW"
consumer_secret="vw0afGtwlxW5sX2EyPsUwcb6jiEurMWoejAbKEAZ6sqxb3l1dK"
access_token="101055484-iQg2OaLNtEHInKDPX2Ca6MrCBkBZ74G94iHAPgJm"
access_token_secret="Zbs11KOBiy9mMQTBYtwnfy1fKFnnzP57hNLEntqtQ9abs"

def register_handler(dest):
    global response_dest
    response_dest = dest
    """ This is for erlport, so it's send message back to Erlang """
    def handler(message):
        if isinstance(message, tuple) and len(message) == 2:
            if message[0] == 'start':
                start(message[1].to_string())
        elif message == ["response", data]:
            cast(dest, data)
    set_message_handler(handler)
    return Atom("ok")

class StdOutListener(StreamListener):
    """ A listener handles tweets are the received from the stream.
    This is a basic listener that just prints received tweets to stdout.

    """
    def on_data(self, data):
        cast(response_dest, data)
        return True

    def on_error(self, status):
        cast(response_dest, List([Atom("error"), status]))

# Use the below if you wan't to call from CMD line
# if __name__ == '__main__':
def start(filterStrAsci):
    l = StdOutListener()
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    stream = Stream(auth, l)
    stream.filter(track=[filterStrAsci])
