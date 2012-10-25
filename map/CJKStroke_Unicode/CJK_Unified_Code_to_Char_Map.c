#include <stdio.h>
#include <iconv.h>

int main(int argc, char **argv){
	int i = 0x4E00;
	iconv_t conv = iconv_open("UTF-8", "UTF-16");
	for( i = 0x4E00;i <= 0x9FFF; i++){
		 iconv (conv,  **inbuf, size_t *inbytesleft, char **outbuf, size_t *outbytesleft);	
	}
}
