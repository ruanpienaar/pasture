#!/bin/bash
H=`hostname -f`
export PAS_MASTER_DB_NODE="'p1@$H'"
export PAS_CLUSTER_DB_NODES="['p1@$H']"
#echo $PAS_MASTER_DB_NODE
#echo $PAS_CLUSTER_DB_NODES
cd rel/files && rebar create -f template=sys.config master_db_node="$PAS_MASTER_DB_NODE" cluster_nodes="$PAS_CLUSTER_DB_NODES" db_mod="pasture_db_esqlite"
