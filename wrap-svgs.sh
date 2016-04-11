#!/bin/bash

cat <<EOF
<html><head>
<title>diagrams</title>
<style>
.diagram {
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
cat <<EOF
    <object class="diagram" type="image/svg+xml" data="$src"></object>
EOF
done
echo '</body></html>'
