#include "wasm-rt.h"

#include <stdio.h>
#include <stdlib.h>

jmp_buf wasm_rt_jmp_buf;
volatile int wasm_rt_jmp_buf_valid = 0;

void wasm_rt_trap(void) {
    if (wasm_rt_jmp_buf_valid) {
        wasm_rt_jmp_buf_valid = 0;
        longjmp(wasm_rt_jmp_buf, 1);
    }

    fputs("wasm2c-rocq: trap\n", stderr);
    abort();
}
