fid = stdin()
while ~feof(fid)
  line = fgets(fid);  % Read a line
  printf("%s", line);
endwhile
