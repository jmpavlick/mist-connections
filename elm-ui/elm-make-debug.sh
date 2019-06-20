#!/bin/bash

# make it easy to see where the messages start
printf "\n\n\n\n\n\n\n\n\n\n"

# faster than gulp? check.
output=$(elm make src/Main.elm --debug --output ../static/Main.js)

echo "${output}" 

if [[ "${output}" = *'Success'* ]]; then
	say "Build succeeded" 
	grep -nrHI TODO src/*.elm
else
	say "Build failed."
fi

printf "\n\n>>>> Build completed at `date`\n\n"
