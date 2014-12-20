#!/bin/sh
cd `dirname $0`

erlc reloader.erl -o reloader.beam

exec erl -name pasture@192.168.1.80 -config $PWD/rel/pasture/releases/1/sys.config -pa $PWD/apps/*/ebin $PWD/deps/*/ebin $PWD/tests $PWD \
-boot start_sasl -mnesia dir "'"$PWD"/Mnesia'" -setcookie pasture_dev -s pasture_app start 127.0.0.1 -heart -detached -noinput -smp 1:1