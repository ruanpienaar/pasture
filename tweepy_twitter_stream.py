#!/usr/bin/env python

# INSTALL:
# pip install tweepy
# pip install erlport

from __future__ import absolute_import, print_function

from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

from erlport.erlterms import Atom
# from erlport.erlang import set_message_handler, cast
from erlport.erlang import cast


# Go to http://apps.twitter.com and create an app.
# The consumer key and secret will be generated for you after
consumer_key="VxVSRMkD97tJSm3iBg0fbatBR"
consumer_secret="UGfTwIh1DRM0xKQYGtYQjV8maSmiTJSfghmT1KKunU1K5rK03l"

# After the step above, you will be redirected to your app's page.
# Create an access token under the the "Your access token" section
access_token="101055484-NVfKL3Tr1LioLunboyIvqPW8z4sE8hgVUlJkVRYB"
access_token_secret="EBoeXKuK3oLWEExK6otVGEzc7hXgF50ZJhBZBNICbifQV"

class StdOutListener(StreamListener):
    """ A listener handles tweets are the received from the stream.
    This is a basic listener that just prints received tweets to stdout.

    """
    def on_data(self, data):
        # print(data)
        # send response back to erlang
        cast(Atom("python_msg_receiver"), data)
        return True

    def on_error(self, status):
        # send error message to erlang, so we can restart
        cast(Atom("python_msg_receiver"), Atom("error"))
        print(status)

# if __name__ == '__main__':
def start(filterStrAsci):
    filterStr = ''.join(chr(i) for i in filterStrAsci)
    l = StdOutListener()
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    stream = Stream(auth, l)
    stream.filter(track=[filterStr])