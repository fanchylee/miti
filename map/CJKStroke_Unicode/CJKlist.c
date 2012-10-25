/*
 *generate a list of CJK unified Ideographs' ASCII unicode number
 */
#include <stdio.h>
int main(int argc, char **argv){
	int i = 0x4E00;
	for( i = 0x4E00;i <= 0x9FFF; i++){
		printf("(%X\n", i);
	}
}
