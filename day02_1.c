#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv) {
	char ch;
	char buf[100];
	int bufptr;
	int min, max, sum, n;
	min = max = -1;
	bufptr = 0;
	sum = 0;
	while(read(STDIN_FILENO, &ch, 1) > 0) {
		if (ch == '\t' || ch == ' ' || ch == '\n') {
			buf[bufptr] = '\0';
			bufptr = 0;
			sscanf(&buf[0], "%d", &n);
			if (min == -1 && max == -1) {
				min = max = n;
			} else if (n < min) {
				min = n;
			} else if (n > max) {
				max = n;
			}
			if (ch == '\n') {
				sum += abs(min - max);
				min = max = -1;
				bufptr = 0;
			}
		} else {
			buf[bufptr++] = ch;
		}
	}
	printf("%d\n", sum);
	return 0;
}
