function day13 ()
  input = readInput();
  partOne = calculatePrice(input);
  printf("%d\n", partOne);

  partTwo = calculatePrice(input, 10000000000000, 0);
  printf("%d\n", partTwo);
end

function lines = readInput ()
  lines = [];
  fid = stdin();
  while ~feof(fid)
    line = fgets(fid);  % Read a line
    buttonA = textscan(line, 'Button A: X+%n, Y+%n');
    line = fgets(fid);  % Read a line
    buttonB = textscan(line, 'Button B: X+%n, Y+%n');
    line = fgets(fid);  % Read a line
    prize = textscan(line, 'Prize: X=%n, Y=%n');
    AX = buttonA{1};
    AY = buttonA{2};
    BX = buttonB{1};
    BY = buttonB{2};
    PX = prize{1};
    PY = prize{2};
    line = fgets(fid);
    lines = [lines; AX, AY, BX, BY, PX, PY];
  end
end

function totalPrice = calculatePrice (machines, offset = 0, enforceButtonPressLimit = 1)
  totalPrice = 0;
  for r = 1:rows(machines)
    AX = machines(r, 1);
    AY = machines(r, 2);
    BX = machines(r, 3);
    BY = machines(r, 4);
    PX = machines(r, 5) + offset;
    PY = machines(r, 6) + offset;

    aPresses = (BY * PX - BX * PY) / (AX * BY - AY * BX);
    bPresses = (AY * PX - AX * PY) / (AY * BX - AX * BY);

    if aPresses == floor(aPresses) && bPresses == floor(bPresses) && (enforceButtonPressLimit == 0 || (aPresses < 101 && bPresses < 101));
      price = aPresses * 3 + bPresses;
      totalPrice = totalPrice + price;
    end
  end
end
