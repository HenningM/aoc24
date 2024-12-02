#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    char *buffer = NULL;
    int read;
    unsigned long len;
    read = getline(&buffer, &len, stdin);
    if (-1 != read)
        puts(buffer);
    else
        printf("No line read...\n");

    printf("Size read: %d\n Len: %lu\n", read, len);
    free(buffer);
    return 0;
}
