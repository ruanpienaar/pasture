# http://docs.resin.io/#/pages/dockerfile.md

FROM resin/rpi-raspbian:wheezy-2015-02-08

RUN apt-get -q update && apt-get install -y erlang

COPY . /app

CMD ["cd /app"]
CMD ["make"]
CMD ["rel/pasture/bin/pasture", "start"]