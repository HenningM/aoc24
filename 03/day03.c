#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <string.h>

#define MUL_PATTERN "mul\\(([0-9]+),([0-9]+)\\)"
#define DO_PATTERN "do\\(\\)"
#define DONT_PATTERN "don't\\(\\)"

int main(int argc, char *argv[])
{
    char *buffer = NULL;
    char *cursor;
    regex_t mul_re;
    regex_t do_re;
    regex_t dont_re;
    regmatch_t mul_rm[3];
    regmatch_t do_rm[1];
    regmatch_t dont_rm[1];
    int read;
    unsigned long len;
    int enabled = 1;
    unsigned long sum = 0;
    unsigned long sump2 = 0;

    if (regcomp(&mul_re, MUL_PATTERN, REG_EXTENDED) != 0)
    {
        fprintf(stderr, "Failed to compile regex '%s'\n", MUL_PATTERN);
        return EXIT_FAILURE;
    }

    if (regcomp(&do_re, DO_PATTERN, REG_EXTENDED) != 0)
    {
        fprintf(stderr, "Failed to compile regex '%s'\n", DO_PATTERN);
        return EXIT_FAILURE;
    }

    if (regcomp(&dont_re, DONT_PATTERN, REG_EXTENDED) != 0)
    {
        fprintf(stderr, "Failed to compile regex '%s'\n", DONT_PATTERN);
        return EXIT_FAILURE;
    }

    while (-1 != getline(&buffer, &len, stdin)) {
        cursor = buffer;
        while (regexec(&mul_re, cursor, 3, mul_rm, 0) == 0)
        {
            if (enabled == 1) {
              if (regexec(&dont_re, cursor, 1, dont_rm, 0) == 0 && dont_rm[0].rm_so < mul_rm[0].rm_so) {
                  enabled = 0;
                  cursor += dont_rm[0].rm_eo;
                  continue;
              }
            } else {
              if (regexec(&do_re, cursor, 1, do_rm, 0) == 0 && do_rm[0].rm_so < mul_rm[0].rm_so) {
                  enabled = 1;
                  cursor += do_rm[0].rm_eo;
                  continue;
              }
            }

            unsigned int rm1len = mul_rm[1].rm_eo - mul_rm[1].rm_so;
            unsigned int rm2len = mul_rm[2].rm_eo - mul_rm[2].rm_so;
            char rm1str[rm1len + 1];
            char rm2str[rm2len + 1];
            char *rm1start = cursor + mul_rm[1].rm_so;
            char *rm2start = cursor + mul_rm[2].rm_so;
            strncpy(rm1str, rm1start, rm1len);
            rm1str[rm1len] = '\0';
            strncpy(rm2str, rm2start, rm2len);
            rm2str[rm2len] = '\0';
            unsigned int rm1 = atoi(rm1str);
            unsigned int rm2 = atoi(rm2str);
            sum += rm1 * rm2;
            if (enabled == 1) {
              sump2 += rm1 * rm2;
            }
            cursor += mul_rm[0].rm_eo;
        }
    }

    printf("%lu\n", sum);
    printf("%lu\n", sump2);

    free(buffer);
    return 0;
}
