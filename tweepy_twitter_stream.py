#!/usr/bin/env python

# INSTALL:
# pip install tweepy
# pip install erlport

from __future__ import absolute_import, print_function

from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast

# from threading import Thread
# from time import sleep

tweets = []
consumer_key="VxVSRMkD97tJSm3iBg0fbatBR"
consumer_secret="UGfTwIh1DRM0xKQYGtYQjV8maSmiTJSfghmT1KKunU1K5rK03l"
access_token="101055484-NVfKL3Tr1LioLunboyIvqPW8z4sE8hgVUlJkVRYB"
access_token_secret="EBoeXKuK3oLWEExK6otVGEzc7hXgF50ZJhBZBNICbifQV"
# erlang_reg_proc = ''

def register_handler(dest):
    """ This is for erlport, so it's send message back to Erlang """
    # if erlang_reg_proc == '':
    #     erlang_reg_proc = dest

    def handler(message):
        # Handle when list is empty and can't be poped
        print(message)
        if message == 'pop':
            cast(dest, tweets.pop())
        elif message == 'status':
            cast(dest, len(tweets))
        elif isinstance(message, tuple) and len(message) == 2:
            if message[0] == 'start':
                # print( message[1].to_string() )

                # Start a thread here with message[1]

                cast(dest, Atom("ok"))

    set_message_handler(handler)
    return Atom("ok")

class StdOutListener(StreamListener):
    """ A listener handles tweets are the received from the stream.
    This is a basic listener that just prints received tweets to stdout.

    """
    def on_data(self, data):
        tweets.append(data)
        return True

    def on_error(self, status):
        print(status)

# Use the below if you wan't to call from CMD line
# if __name__ == '__main__':
def start(filterStrAsci):
    # filterStr = ''.join(chr(i) for i in filterStrAsci)
    l = StdOutListener()
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    stream = Stream(auth, l)
    stream.filter(track=[filterStr])

# def threaded_function(arg):
    # for i in range(arg):
    #     print("running")
    #     sleep(1)

# Use the below if you wan't to call from CMD line
# if __name__ == "__main__":
# def thread_start(filterStrAsci):
#     thread = Thread(target = start, args = (filterStrAsci))
#     thread.start()
#     thread.join()
#     print("thread finished...exiting")