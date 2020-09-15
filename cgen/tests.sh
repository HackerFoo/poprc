echo "__ tests.hello"
bash ./poprc tests.hello <<EOF
Dusty
EOF

echo
echo "__ tests.calc"
bash ./poprc tests.calc <<EOF
3
4
*
fib
q
EOF
