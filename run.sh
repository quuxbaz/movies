#!/bin/sh
nohup /home/dbastos/racket/bin/racket -l errortrace -t movies.rkt -- --production > logs/run.log 2>&1 &
