BEGIN {
  scanning = 1;
  printing = (mode ~ "header") ? 1 : 0;
}

{ toggles = 1 }

(FNR == 1) && printing {
  printf "#line %d \"%s\"\n", FNR, FILENAME;
}

/^[[:space:]]*\/\*/,/\*\// {
  toggles = 0;
}

/^[[:space:]]*$/ || /^[[:space:]]*\/\/.*/ || /^\#.*$/ {
  toggles = 0;
}

scanning && toggles {
  scanning = 0;
  printing = !printing;
  if (printing) {
    printf "#line %d \"%s\"\n", FNR, FILENAME;
  }
}

printing
