# http://docs.resin.io/#/pages/dockerfile.md

FROM resin/rpi-raspbian:wheezy-2015-02-08

RUN apt-get -q update && apt-get install -y erlang

RUN apt-get update \
	&& apt-get install -y erlang && apt-get install -y ssh \
	# Remove package lists to free up space 
	&& rm -rf /var/lib/apt/lists/*

COPY . /app
# ADD . /app
# CMD ["bash", /app/rel/pasture/bin/pasture", "start"]