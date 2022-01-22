#include <stdlib.h>

#include "plml.h"

Value
mkint(long n)
{
	return (Value)(n << 1 | 1);
}

Value
mkclosure(Fun f, Env env)
{
	Value v;

	v = malloc(sizeof(Closure));
	((Closure *)v)->ref = 1;
	((Closure *)v)->env = env;
	((Closure *)v)->f = f;
	return v;
}

Value
call_closure(Closure f, Value x)
{
	return f.f(x, f.env);
}

Env
alloc_env(size_t s)
{
	return malloc(s * sizeof(Value));
}

void
add_env(Env e, Value v, int n)
{
	e[n] = v;
}

void
drop(Value v)
{
	switch (((long)v) & 0b111) {
	case 0:
		((Closure *)v)->ref -= 1;
		if (((Closure *)v)->env)
			free(((Closure *)v)->env);
		break;
	case 1:
		break;
	}
}

void
dup(Value v)
{
}

