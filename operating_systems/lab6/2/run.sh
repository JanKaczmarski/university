#!/bin/zsh

set -eo pipefail

cleanup() {
    echo "[run.sh] running cleanup"
    rm -f master worker to_master to_worker
}
trap cleanup EXIT INT TERM

echo "[run.sh] Compilation"
gcc master.c -o master
gcc worker.c -o worker


mkfifo to_worker to_master

echo "[run.sh] Starting worker"
./worker &

echo "[run.sh] Starting master"
echo "-20 20" | ./master


sleep 0.1

wait
