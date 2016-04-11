#!/bin/bash

cat <<EOF
<html><head>
<title>diagrams</title>
<style>
svg {
  margin: 0.5em;
}
body {
  background-color: #d0d0d0;
}
</style>
</head><body>
EOF
for src in "$@"
do
    tail -n +7 "$src"
done
echo '</body></html>'
