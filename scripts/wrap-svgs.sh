#!/bin/bash

cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/strict.dtd">
<html xmlns="http://www.w3.org/TR/xhtml1/strict" ><html><head>
<title>diagrams</title>
<style>
svg {
  margin: 0.5em;
  border: 1px solid #404040;
}
body {
  background-color: #000000;
}
</style>
</head><body>
EOF
for src in "$@"
do
    tail -n +7 "$src"
done
echo '</body></html>'
