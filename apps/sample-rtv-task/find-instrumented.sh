#!/bin/sh

# XXX pattern match out spaces
function findLns {
  find . -name "*.c" | xargs grep "__attribute__((instrument" --no-filename
}

# make lines of > type, index, name
function typeIdxName {
  findLns | awk '{ \
    sub("__attribute__\\(\\(instrument\\(", "", $3); \
    sub("\\)\\)\\)", "", $3); \
    sub(";", "", $3); \
    print $1, $3, $2; \
  }'
}

function toType {
  typeIdxName | awk '{ \
    sub("int32_t"  , "Int",     $1); \
    sub("int"      , "Int",     $1); \
    sub("long int" , "LongInt", $1); \
    sub("double"   , "Double",  $1); \
    sub("float"    , "Float",   $1); \
    sub("char"     , "Char",    $1); \
    print $1, $2, $3; \
  }'
}

# sort the entries on the instrument index
function sortIt {
  toType | sort --key=2 -n
}

# XXX need to check that no index has been duplicated!

function module {
  sortIt | awk \
    'BEGIN { \
       NUM=0; \
       print "module Variables where\n\n\
import Ivory.RTVerification.GenSettersGetters (Type(..))\n\n\
variables :: [Type]\nvariables =\n\t["; \
     } \
     { # print "\t", $1, "//", $3; \
       type[NUM] = $1; \
       sym[NUM] = $3; \
       NUM++; \
     } \
     END { \
       # Print first line without comma seperator \
       print "\t  ", type[0], " // ", sym[0]; \
       # Print remaining lines with seperators \
       for(i=1; i<NR; i++) { \
         print "\t, ", type[i], " // ", sym[i]; \
       } \
       print "\t]";}'
}

module
