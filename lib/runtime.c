#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int readInt()
{
    int x;
    scanf("%d\n", &x);
    return x;
}

char *readString()
{
    char *line = NULL;
    size_t len = 0;
    ssize_t lineSize = 0;
    lineSize = getline(&line, &len, stdin);
    len = strlen(line);
    line[len - 1] = '\0';
    return line;
}

void printInt(int x)
{
    printf("%d\n", x);
}

void printString(char *s)
{
    puts(s);
}

char *__concat(char *s1, char *s2)
{
    char *s3 = malloc(strlen(s1) + strlen(s2) + 1);
    char *s4 = strcpy(s3, s1);
    return strcat(s4, s2);
}