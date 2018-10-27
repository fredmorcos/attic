[BITS 16]

[ORG 0x7C00]
	mov	ax,	[label]

label:
	dw	0

	mov	ah,	0Eh
	mov	al,	'W'
	mov	bh,	0Fh
	mov	bl,	0
	int	10h

hang:
	jmp	hang

	times	510-($-$$)	db	0
	dw	0AA55h