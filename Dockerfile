FROM resin/rpi-raspbian:wheezy-2015-06-03

# Update Base System

RUN 		apt-get update
RUN 		apt-get -y upgrade

# Set Environment Variables

ENV 		HOME /root
# Set Language Environment

#RUN 		apt-get install -y language-pack-en

#ENV 		LANGUAGE en_US.UTF-8
#ENV 		LANG en_US.UTF-8
#ENV 		LC_ALL en_US.UTF-8
#RUN 		locale-gen en_US.UTF-8
#RUN 		dpkg-reconfigure locales
#ENV 		DEBIAN_FRONTEND noninteractive

# Install Basic Packages

RUN 		apt-get install -y build-essential software-properties-common
RUN 		apt-get install -y wget curl git man unzip screen


# Install Erlang
WORKDIR 	/tmp

RUN 		wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
RUN 		dpkg -i erlang-solutions_1.0_all.deb 
RUN 		apt-get update 
RUN 		apt-get install -y erlang incron
RUN 		apt-get clean
RUN 		rm -rf /tmp/*
RUN 		mkdir /code
 

# Install Erlang Application

RUN 		git clone https://github.com/ruanpienaar/pasture /code/app/
WORKDIR 	/code/app
RUN 		make rel

# setup ssh keys

EXPOSE 		8001 21
CMD 		["/code/app/pasture/rel/pasture/bin/pasture", ""]














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


