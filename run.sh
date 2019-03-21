#!/bin/sh
nohup /usr/bin/racket -l errortrace -t movies.rkt -- --key "$1" --production > logs/run.log 2>&1 &
