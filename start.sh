#!/bin/bash

cd elm-ui
./elm-make-debug.sh
open -a Safari.app http://localhost:5000
fswatch -o ./src/ | xargs -n1 -I{} ./elm-make-debug.sh &

python3 ../application.py &
