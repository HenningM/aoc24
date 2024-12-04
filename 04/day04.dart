import 'dart:io';

var target_word = "XMAS";
var target_word_p2 = "MAS";
var steps = [
  [-1, -1],
  [-1, 0],
  [-1, 1],
  [0, -1],
  [0, 1],
  [1, -1],
  [1, 0],
  [1, 1]
];

List<List<String>> readInput() {
  List<List<String>> lines = [];

  while (true) {
    var line = stdin.readLineSync();
    if (line == null) {
      return lines;
    }
    var trimmedLine = line.trim();
    if (trimmedLine.isEmpty) continue;
    
    lines.add(trimmedLine.split(""));
  }
}

bool find_needle(String needle, List<List<String>> haystack, int xPos, int yPos, int needlePos, List<int> step) {
  if (needlePos >= needle.length) {
    return true;
  }
  if (yPos < 0 || xPos < 0) {
    return false;
  }
  if (yPos >= haystack.length || xPos >= haystack[yPos].length) {
    return false;
  }
  if (needle.split("")[needlePos] == haystack[yPos][xPos]) {
    return find_needle(needle, haystack, xPos + step[0], yPos + step[1], needlePos + 1, step);
  }
  return false;
}

int count_occurrences(String needle, List<List<String>> haystack) {
  var count = 0;
  for (var y = 0; y < haystack.length; y++) {
    for (var x = 0; x < haystack[y].length; x++) {
      for (var step in steps) {
        if (find_needle(needle, haystack, x, y, 0, step)) {
          count++;
        }
      }
    }
  }
  return count;
}

int count_crosses(String needle, List<List<String>> haystack) {
  var needle_rev = needle.split("").reversed.join();
  var count = 0;
  for (var y = 0; y < haystack.length; y++) {
    for (var x = 0; x < haystack[y].length; x++) {
      var fw = find_needle(needle, haystack, x, y, 0, [1, 1]);
      var bw = find_needle(needle_rev, haystack, x, y, 0, [1, 1]);
      if (fw || bw) {
        var fw2 = find_needle(needle, haystack, x + 2, y, 0, [-1, 1]);
        var bw2 = find_needle(needle_rev, haystack, x + 2, y, 0, [-1, 1]);
        if (fw2 || bw2) {
          count++;
        }
      }
    }
  }
  return count;
}

void main() {
    List<List<String>> input = readInput();
    var occurrences = count_occurrences(target_word, input);
    var crosses = count_crosses(target_word_p2, input);

    print(occurrences);
    print(crosses);
}
