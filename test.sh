#!/bin/bash
# file: test.sh

cd server
stack build
stack exec server &
server_pid=$!
sleep 1
cd ..

npm run test

kill -9 $server_pid
