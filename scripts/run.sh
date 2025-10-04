#!/bin/bash

FILES="main.hs shoggothstaring.cabal"
CMD="cabal run shoggothstaring rebuild;notify-send "Compiled"; cabal run shoggothstaring watch"

RESTART_DELAY=1
WATCH_PID=""

kill_pid() {
  local pid=$1
  if [ ! -z "$pid" ]; then
    echo "Stopping processes related to $pid..."

    # Send TERM to the process tracked by $! (the shell)
    kill -TERM "$pid" 2>/dev/null

    # Aggressively kill the actual server process by command line
    pkill -f shoggothstaring 2>/dev/null

    wait "$pid" 2>/dev/null
    echo "Cleanup complete."
  fi
}

cleanup() {
  kill_pid "$WATCH_PID"
  exit 0
}

trap cleanup INT TERM

echo "Starting cabal build/watch cycle..."

while true; do
  (eval "$CMD") &
  WATCH_PID=$!
  echo "Running command with PID: $WATCH_PID"

  echo "Watching for changes in $FILES..."
  inotifywait -e close_write,delete,move -q $FILES

  if [ ! -z "$WATCH_PID" ]; then
    echo "File change detected."
    kill_pid "$WATCH_PID"
  fi

  WATCH_PID=""
  echo "Restarting in $RESTART_DELAY second(s)..."
  sleep $RESTART_DELAY
done
