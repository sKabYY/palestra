ALL: a.out

a.out: runtime.c _local_t.s
	gcc runtime.c _local_t.s

_local_t.s: compiler.ss
	scheme --script compiler.ss

clean:
	rm _local_t.s a.out
