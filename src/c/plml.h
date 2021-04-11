#ifndef __PLML_H_
#define __PLML_H_

#include <stddef.h>

typedef void **Env;
typedef void *Value;
typedef Value (*Fun)(Value, Env);
struct closure {
	Fun f;
	Env env;
};
typedef struct closure Closure;

Value mkint(long);
Value mkclosure(Fun, Env);

Value call_closure(Closure, Value);

Env alloc_env(size_t);
void add_env(Env, Value, int);

#endif
