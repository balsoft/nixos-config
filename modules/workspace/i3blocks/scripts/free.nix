{ bc, ... }:
''
echo `free | tail -2 | head -1 | awk '{print "scale=3; "$7"/1000000"}' | ${bc}/bin/bc -l`GB
''
