#include <stdio.h>

const int START_ADDR = 0x00001000;

int main(){
    *(char*)START_ADDR = 0x01;
    *(char*)START_ADDR = 0x00;
    *(char*)START_ADDR = 0x01;
    *(char*)START_ADDR = 0x00;
    *(char*)START_ADDR = 0x01;
    *(char*)START_ADDR = 0x00;
    *(char*)START_ADDR = 0x01;
    asm volatile("unimp");
    return 0;
}