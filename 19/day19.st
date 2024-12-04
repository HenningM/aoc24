| input line |
input := FileStream stdin.
[input atEnd] whileFalse: [
    line := FileStream stdin nextLine.
    FileStream stdout nextPutAll: line; nl.
].

