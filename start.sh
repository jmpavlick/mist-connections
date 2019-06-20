#!/bin/bash

ps ax | grep Python | grep [m]ist-connections | awk '{ print $1 }' | xargs kill

python3 ../mist-connections.py &

cd elm-ui

./elm-make-debug.sh
open -a Safari.app http://localhost:5000
fswatch -o ./src/ | xargs -n1 -I{} ./elm-make-debug.sh
