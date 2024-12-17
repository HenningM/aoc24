while read line
do
  echo "$line" >> input.tmp
done < "${1:-/dev/stdin}"

regA=`cat input.tmp | grep "Register A" | sed "s/Register A: \([0-9]\+\)/\1/"`
regB=`cat input.tmp | grep "Register B" | sed "s/Register B: \([0-9]\+\)/\1/"`
regC=`cat input.tmp | grep "Register C" | sed "s/Register C: \([0-9]\+\)/\1/"`
program=`cat input.tmp | grep "Program" | sed "s/Program: \([0-9,]\+\)/\1/"`
qsi --use day17.qs --entry "Quantum.Day17.RunP1($regA, $regB, $regC, [$program])" --exec
qsi --use day17.qs --entry "Quantum.Day17.RunP2($regA, $regB, $regC, [$program])" --exec
rm input.tmp
