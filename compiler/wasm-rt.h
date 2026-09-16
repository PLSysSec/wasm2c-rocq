#ifndef WASM_RT_H
#define WASM_RT_H

#include <setjmp.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__GNUC__) || defined (__clang__)
#define WASM_RT_NORETURN __attribute__((noreturn))
#elif defined(_MSC_VER)
# define WASM_RT_NORETURN __declspec(noreturn)
#else
#define WASM_RT_NORETURN
#endif

extern jmp_buf wasm_rt_jmp_buf;
extern volatile int wasm_rt_jmp_buf_valid;

#ifdef __cplusplus
}
#endif

#endif
