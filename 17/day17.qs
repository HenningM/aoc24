namespace Quantum.Day17 {
    import Std.Arrays.*;

    struct Machine {
        regA: Int,
        regB: Int,
        regC: Int,
        program: Int[],
        pCnt: Int,
        out: Int[]
    }

    function ProcessAdvInstruction(machine: Machine, op: Int): Machine {
        let newRegA = machine.regA / (2 ^ op);
        return new Machine { ...machine, regA = newRegA, pCnt = machine.pCnt + 2 };
    }

    function ProcessBxlInstruction(machine: Machine, op: Int): Machine {
        return new Machine { ...machine, pCnt = machine.pCnt + 2, regB = machine.regB ^^^ op };
    }

    function ProcessBstInstruction(machine: Machine, op: Int): Machine {
        return new Machine { ...machine, pCnt = machine.pCnt + 2, regB = op % 8 };
    }

    function ProcessJnzInstruction(machine: Machine, op: Int): Machine {
        if machine.regA == 0 {
            return new Machine { ...machine, pCnt = machine.pCnt + 2 };
        }
        return new Machine { ...machine, pCnt = op };
    }

    function ProcessBxcInstruction(machine: Machine, op: Int): Machine {
        return new Machine { ...machine, pCnt = machine.pCnt + 2, regB = machine.regB ^^^ machine.regC };
    }

    function ProcessOutInstruction(machine: Machine, op: Int): Machine {
        let newOut = machine.out + [op % 8];
        return new Machine { ...machine, out = newOut, pCnt = machine.pCnt + 2 };
    }

    function ProcessBdvInstruction(machine: Machine, op: Int): Machine {
        let newRegB = machine.regA / (2 ^ op);
        return new Machine { ...machine, regB = newRegB, pCnt = machine.pCnt + 2 };
    }

    function ProcessCdvInstruction(machine: Machine, op: Int): Machine {
        let newRegC = machine.regA / (2 ^ op);
        return new Machine { ...machine, regC = newRegC, pCnt = machine.pCnt + 2 };
    }

    function GetComboOp(machine: Machine, op: Int): Int {
        if op < 4 {
            return op;
        }
        if op == 4 {
            return machine.regA;
        }
        if op == 5 {
            return machine.regB;
        }
        if op == 6 {
            return machine.regC;
        }
        Message("Invalid ComboOp detected!");
        return op;
    }

    function ProcessInstruction(machine: Machine) : Machine {
        let instr: Int = machine.program[machine.pCnt];
        let op: Int = machine.program[machine.pCnt+1];
        if instr == 0 {
            return ProcessAdvInstruction(machine, GetComboOp(machine, op));
        }
        if instr == 1 {
            return ProcessBxlInstruction(machine, op);
        }
        if instr == 2 {
            return ProcessBstInstruction(machine, GetComboOp(machine, op));
        }
        if instr == 3 {
            return ProcessJnzInstruction(machine, op);
        }
        if instr == 4 {
            return ProcessBxcInstruction(machine, op);
        }
        if instr == 5 {
            return ProcessOutInstruction(machine, GetComboOp(machine, op));
        }
        if instr == 6 {
            return ProcessBdvInstruction(machine, GetComboOp(machine, op));
        }
        if instr == 7 {
            return ProcessCdvInstruction(machine, GetComboOp(machine, op));
        }
        let newMachine = new Machine { ...machine, pCnt = machine.pCnt + 2 };
        return newMachine
    }

    function RunMachineProgram(machine: Machine): Machine {
        mutable m = machine;
        while m.pCnt < Length(m.program) - 1 {
            set m = ProcessInstruction(m);
        }
        return m;
    }

    operation RunP1(regA: Int, regB: Int, regC: Int, program: Int[]) : String {
        mutable machine = new Machine { regA = regA, regB = regB, regC = regC, program = program, pCnt = 0, out = [] };
        set machine = RunMachineProgram(machine);
        mutable out = "";
        for i in machine.out {
            if out == "" {
                set out = $"{i}";
            } else {
                set out = $"{out},{i}";
            }
        }
        return out;
    }

    function FindPossibleA(machine: Machine, code: Int, prev: Int): Int[] {
        mutable possible = [];
        mutable aVal = 0;
        mutable modMachine = machine;
        for n in 0..7 {
            set aVal = n + (prev * 8);
            set modMachine = new Machine { ...machine, regA = aVal };
            set modMachine = RunMachineProgram(modMachine);
            let out = modMachine.out;
            if out[0] == code {
                set possible += [aVal];
            }
        }
        return possible;
    }

    operation RunP2(regA: Int, regB: Int, regC: Int, program: Int[]) : String {
        mutable prev = [0];
        mutable cur: Int[] = [];
        mutable machine = new Machine { regA = regA, regB = regB, regC = regC, program = program, pCnt = 0, out = [] };
        for c in Reversed(machine.program) {
            set cur = [];
            for p in prev {
                set cur += FindPossibleA(machine, c, p);
            }
            set prev = cur;
        }
        for c in cur {
            mutable m = new Machine { ...machine, regA = c };
            set m = RunMachineProgram(m);
            if m.out == m.program {
                return $"{c}";
            }
        }
        return "Not found";
    }
}
