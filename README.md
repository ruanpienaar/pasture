pasture
=======

$ clone repo

$ make rel

$ rel/pasture/bin/pasture console

##View Meetup rsvp's 

http://localhost:8001

##Start a twitter public stream:

You'd need to install these first:
$ pip install tweepy
$ pip install erlport

pasture_twitter_sup:start_child(Str).

The pasture_twitter Mnesia table has the tweets for Str.

Twitter treds page underway 
