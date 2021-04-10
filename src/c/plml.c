#include <stdlib.h>

#include "plml.h"

Value
mkint(long n)
{
	return (Value)n;
}

Value
mkclosure(Fun f, Env env)
{
	Value v;

	v = malloc(sizeof(Closure));
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
