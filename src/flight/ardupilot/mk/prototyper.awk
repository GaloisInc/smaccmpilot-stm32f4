BEGIN {
  RS="{";
  type       = "((\\n)|(^))[[:space:]]*[[:alnum:]_]+[[:space:]]+";
  qualifiers = "([[:alnum:]_\\*&]+[[:space:]]*)*";
  name       = "[[:alnum:]_]+[[:space:]]*";
  args       = "\\([[:space:][:alnum:]_,&\\*\\[\\]]*\\)";
  bodycuddle = "[[:space:]]*$";
  pattern    = type qualifiers name args bodycuddle;
}

match($0, pattern) {
  proto = substr($0, RSTART, RLENGTH);
  gsub("\n", " ", proto);
  printf "%s;\n", proto;
}
