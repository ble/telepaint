#!/bin/sh

url=http://plovr.googlecode.com/files/plovr-c047fb78efb8.jar
sha1=a532d9d65325a700934628f63c30e66d45df3fc7
file=plovr.jar

if [ ! -e $file ]
then
  curl -o $file $url ;
fi;
hash=$(openssl sha1 $file)
if [ "SHA1($file)= $sha1" = "${hash}" ];
then
  echo "ok";
else
  echo "sha1 mismatch";
  rm $file;
fi

