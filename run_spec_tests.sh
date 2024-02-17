#!/bin/sh

zig build -Doptimize=ReleaseSafe

(cd spec_test; ls *.json) | while read i; do
    p=`./zig-out/bin/zwasmi $i 2>&1 | grep 'test pass' | wc -l`
    a=`grep -c assert_  spec_test/$i`
    echo $i $p $a
done | awk '{
  printf("%-22s %5d %5d  (%3.1lf%%)\n", substr($1, 0, length($1)-5), $2, $3, 100 * $2 / ($3 == 0 ? 1 : $3));
  p+=$2;
  t+=$3
}
END{
  print "------------------------------------------------";
  printf("%-22s %5d %5d  (%3.1lf%%)\n", "", p, t, p * 100 / t)
}'
