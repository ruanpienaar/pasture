#!/bin/bash
if [ -z "$1" ]; then
 echo "usage $0 date (20120631)"
else
 echo "fetching ... $1 ..."
 sleep 1
 curl --insecure -X POST -H "Transfer-Encoding: chunked" --data "ajax=1&pn=p1&htd=$1&htv=l" "https://trends.google.com/trends/hottrends/hotItems" -o $1
fi

