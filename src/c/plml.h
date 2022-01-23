#ifndef __PLML_H_
#define __PLML_H_

#include <stddef.h>

typedef void *Value;
typedef struct {
	int size;
	Value *p;
} Env;

typedef Value (*Fun)(Value, Env);
struct closure {
	int ref;
	Fun f;
	Env env;
};
typedef struct closure Closure;

Value mkint(long);
Value mkclosure(Fun, Env);

Value call_closure(Value, Value);

Env alloc_env(int);
void add_env(Env, Value, int);

void drop(Value);
void dup(Value);

extern Env null_env;

#endif
