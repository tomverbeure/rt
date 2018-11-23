#include <stdint.h>

#include "top_defines.h"

#define LED_CONFIG      *((volatile uint32_t *)(0x00080000 | LED_CONFIG_ADDR  ))

static inline uint32_t rdcycle(void) {
    uint32_t cycle;
    asm volatile ("rdcycle %0" : "=r"(cycle));
    return cycle;
}

void wait(int cycles)
{
#if 1
    volatile int cnt = 0;

    for(int i=0;i<cycles;++i){
        ++cnt;
    }
#else
    int start;

    start = rdcycle();
    while ((rdcycle() - start) <= cycles);
#endif
}


#define WAIT_CYCLES 1000000

int main() {

    LED_CONFIG = 0x00;

    for (;;) {
        wait(WAIT_CYCLES);
        LED_CONFIG = ~0x01;
        wait(WAIT_CYCLES);
        LED_CONFIG = ~0x02;
        wait(WAIT_CYCLES);
        LED_CONFIG = ~0x04;
    }
}
