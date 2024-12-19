Object subclass: TowelDesigner [
    | patterns designCache |

    TowelDesigner class >> with: patterns [
        | td |
        td := self new.
        td init: patterns.
        ^td
    ]

    init: p [
        patterns := p sorted: [:a :b | a size > b size].
        designCache := Dictionary new.
    ]

    canCreateDesign: design [
        design isEmpty ifTrue: [ ^(design isEmpty) ].
        patterns do: [:p |
            | hasPrefix |
            hasPrefix := design startsWith: p.
            hasPrefix ifTrue: [
                | newDesign canCreateSub |
                newDesign := design copyFrom: p size + 1 to: design size.
                canCreateSub := self canCreateDesign: newDesign.
                canCreateSub ifTrue: [ ^canCreateSub ].
            ]
        ].
        ^false
    ]

    countDesignArrangements: design [
        | s isCached |
        design isEmpty ifTrue: [ ^1 ].
        isCached := designCache includesKey: design.
        isCached ifTrue: [ ^designCache at: design ].
        s := (patterns collect: [:p |
            | hasPrefix canCreateSub |
            canCreateSub := 0.
            hasPrefix := design startsWith: p.
            hasPrefix ifTrue: [
                | newDesign |
                newDesign := design copyFrom: p size + 1 to: design size.
                canCreateSub := self countDesignArrangements: newDesign.
            ].
            canCreateSub
        ]) fold: [:a :b | a + b].
        designCache at: design put: s.
        ^s
    ]

    printPatterns [
        patterns do: [:x | Transcript show: x; cr. ]
    ]
]

Object subclass: InputReader [
    readLinesFromStdin [
        | line lines |
        lines := Array new.
        [ line := FileStream stdin nextLine.
          line isNil ifTrue: [ ^lines ].
          lines := lines copyWith: line.
        ] repeat.
    ]

    parsePatterns: pattStr [
        | patterns |
        patterns := pattStr subStrings: ', '.
        ^patterns
    ]

    parseInput [
        | designs firstLine patterns |
        firstLine := FileStream stdin nextLine.
        patterns := self parsePatterns: firstLine.
        FileStream stdin nextLine.
        designs := self readLinesFromStdin.
        ^Array with: patterns with: designs
    ]
]


| input td possibleDesigns possibleDesignArrangements |
input := (InputReader new) parseInput.
td := TowelDesigner with: input first.
possibleDesigns := input second select: [:x | (td canCreateDesign: x)].
Transcript show: possibleDesigns size printString; cr.
possibleDesignArrangements := (possibleDesigns collect: [:x | (td countDesignArrangements: x)]) fold: [:a :b | a + b].
Transcript show: possibleDesignArrangements printString; cr.
