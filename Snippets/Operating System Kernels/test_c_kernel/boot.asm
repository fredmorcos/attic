[BITS 32]
[global start]
[extern _k_main]

start:
	call _k_main

	cli
	hlt
