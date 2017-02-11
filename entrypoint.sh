#!/bin/bash
set -e

echo "[$(date)] Initiating server"

/wait-for-it.sh -s ${PG_HOST}:${PG_PORT}
exec "$@"
