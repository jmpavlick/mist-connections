#!/bin/bash

# make it easy to see where the messages start
printf "\n\n\n\n\n\n\n\n\n\n"

# faster than gulp? check.
output=$(elm make src/Main.elm --debug --output ../static/Main.js)

# play a random and cool message whenever the build succeeds
successMessages=("This will never play because the array is zero-indexed" "Get it son" "Nailed it" "Hallelujah" "Perfect" "You are a god" "Success is yours" "Everything you touch turns to gold" "You are the best" "Everything is cool and good")
messageIndex=$(echo $RANDOM | cut -c1)

echo "${output}" 

if [[ "${output}" = *'Success'* ]]; then
	say ${successMessages[$messageIndex]} 
	grep -nrHI TODO src/*.elm
else
	say "Build failed."
fi

printf "\n\n>>>> Build completed at `date`\n\n"
