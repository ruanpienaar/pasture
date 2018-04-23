#!/bin/sh
set -x
cd `dirname $0`
erl +A 1 +K true -name p1@`hostname` -pa $PWD/apps/*/ebin $PWD/deps/*/ebin $PWD/test -setcookie pasture -s pasture start -config rel/files/sys.config -mnesia dir "'"$PWD"/Mnesia'"