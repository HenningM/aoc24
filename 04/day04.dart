import 'dart:io';

Future<List<String>> readInput() async {
  List<String> lines = [];

  await for (var line in stdin.transform(SystemEncoding().decoder)) {
    var trimmedLine = line.trim();
    if (trimmedLine.isEmpty) continue;
    
    lines.add(trimmedLine);
  }

  return lines;
}

void main() async {
    List<String> input = await readInput();

    for (var line in input) {
        print(line);
    }
}
