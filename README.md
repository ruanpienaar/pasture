pasture
=======

$ clone repo

$ make rel

$ rel/pasture/bin/pasture console

##View Meetup rsvp's 

http://localhost:8001

##Start a twitter public stream:

You'd need to install these first:

get a (Binary package)[http://erlport.org/downloads/#id4] for your erlang
version.

uncompress, and place the erlport DIR in your code:lib_dir/0.

make python_modules

make rel

edit tweepy_twitter_stream with your Twitter App OAUTH keys

start | console

pasture_twitter_sup:start_child(Str).

The pasture_twitter Mnesia table has the tweets for Str.

Twitter treds page underway 
