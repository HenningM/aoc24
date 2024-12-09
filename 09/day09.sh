while read line
do
  echo "$line" >> input.tmp
done < "${1:-/dev/stdin}"

cat input.tmp | io day09p1.io | paste -sd+ | bc
cat input.tmp | io day09p2.io | paste -sd+ | bc
rm input.tmp
