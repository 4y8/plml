#include <stdlib.h>

#include "plml.h"

static void dup_env(Env env);
static void drop_env(Env env);

Env null_env = {.size = 0, .p = NULL};
Value true = (Value)((1 << 3) | 1);
Value false = (Value)((0 << 3) | 1);

static void
dup_env(Env env)
{
	for (int i = 0; i < env.size; ++i)
		dup(env.p[i]);
}

static void
drop_env(Env env)
{
	for (int i = 0; i < env.size; ++i)
		drop(env.p[i]);
}

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
call_closure(Value f, Value x)
{
	Closure *c;
	Value r;

	c = (Closure *)f;
	dup_env(c->env);
	r = c->f(x, c->env);
	drop(f);
	return r;
}

Env
alloc_env(int s)
{
	return (Env){.size = s, .p = malloc(s * sizeof(Value))};
}

void
add_env(Env e, Value v, int n)
{
	e.p[n] = v;
}

void
drop(Value v)
{
	switch (((long)v) & 0b111) {
	case 0: {
		Closure *c = (Closure *)v;
		c->ref -= 1;
		if (c->ref == 0) {
			drop_env(c->env);
			free(c->env.p);
			free(c);
		}
		break;
	}
	case 1:
		break;
	}
}

void
dup(Value v)
{
	switch (((long)v) & 0b111) {
	case 0:
		((Closure *)v)->ref += 1;
		break;
	case 1:
		break;
	}
}
