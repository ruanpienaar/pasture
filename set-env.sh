#!/bin/bash
export PAS_MASTER_DB_NODE="'p1@rpmbp'"
export PAS_CLUSTER_DB_NODES="['p1@rpmbp']"
echo $PAS_MASTER_DB_NODE
echo $PAS_CLUSTER_DB_NODES
cd rel/files && rebar create -f template=sys.config master_db_node="$PAS_MASTER_DB_NODE" cluster_nodes="$PAS_CLUSTER_DB_NODES" db_mod="pasture_db_esqlite"
