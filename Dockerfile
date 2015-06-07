# http://docs.resin.io/#/pages/dockerfile.md

FROM resin/rpi-raspbian:wheezy-2015-06-03

RUN apt-get -q update && apt-get install -y erlang

RUN apt-get update \
	&& apt-get install -y erlang \
	&& apt-get install -y ssh \
	&& apt-get install -y build-essential \
	# Remove package lists to free up space 
	&& rm -rf /var/lib/apt/lists/*

ADD . /app
EXPOSE 21
#WORKDIR /app
#CMD ["bash", "make"]
#CMD ["bash", "/app/rel/pasture/bin/pasture", "start"]