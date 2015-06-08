FROM resin/rpi-raspbian:wheezy-2015-06-03

# Update Base System

RUN 		apt-get update
RUN 		apt-get -y upgrade

# Install Basic Packages

RUN 		apt-get install -y build-essential software-properties-common
RUN 		apt-get install -y wget curl git man unzip screen erlang git

# Install Erlang Application

ADD			. /app

WORKDIR 	/app/
CMD 		["git", "clone", "https://github.com/ruanpienaar/pasture"]
WORKDIR 	/app/pasture
CMD 		["make", "rel"]

# setup ssh keys

EXPOSE 		8001 21
CMD 		["/code/app/pasture/rel/pasture/bin/pasture", "start"]

# http://docs.resin.io/#/pages/dockerfile.md

# FROM resin/rpi-raspbian:wheezy-2015-06-03

# RUN apt-get -q update && apt-get install -y erlang

# RUN apt-get update \
# 	&& apt-get install -y erlang \
# 	&& apt-get install -y ssh \
# 	&& apt-get install -y build-essential \
# 	# Remove package lists to free up space 
# 	&& rm -rf /var/lib/apt/lists/*

# ADD . /app
# EXPOSE 21
#WORKDIR /app
#CMD ["bash", "make"]
#CMD ["bash", "/app/rel/pasture/bin/pasture", "start"]