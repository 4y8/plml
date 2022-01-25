#ifndef __PLML_H_
#define __PLML_H_

#include <stddef.h>

typedef void *Value;
typedef struct {
	Value *p;
	int size;
} Env;

typedef Value (*Fun)(Value, Env);
struct closure {
	Fun f;
	Env env;
	int ref;
};
typedef struct closure Closure;

Value mkint(long);
Value mkclosure(Fun, Env);

Value call_closure(Value, Value);

Env alloc_env(int);
void add_env(Env, Value, int);

void drop(Value);
void dup(Value);

Value primeqint(Value, Env);

Value primaddint(Value, Env);
Value primsubint(Value, Env);
Value primmulint(Value, Env);
Value primdivint(Value, Env);

extern Env null_env;
extern Value true;
extern Value false;

#endif
