#/bin/bash

f=test.log
i=0
e=0
while( [ $e == 0 ] ) do
  i=$((i+1))
  echo "$(date) - $i"
  sbt test > $f
  e=$?
done

tail $f

