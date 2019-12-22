#!/bin/bash

DIR=$(dirname $0)
FILE="$DIR/main.js"
COMPILE_TO="bookmarklet.js"
echo "Compiling " $FILE"..."

echo -n "javascript:(function(){" > $COMPILE_TO
curl --data-urlencod "js_code@$FILE" -d compilation_level=SIMPLE_OPTIMIZATIONS -d output_format=text -d output_info=compiled_code https://closure-compiler.appspot.com/compile | tr '\n' ' ' >> $COMPILE_TO
echo -n 'main()})()' >> $COMPILE_TO

echo "Successfully processed into $COMPILE_TO!"
