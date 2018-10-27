run: kernel.iso bochs.conf
	bochs -f bochs.conf -q

iso kernel.iso: kernel.elf stage2_eltorito menu.lst
	mkdir -p iso/boot/grub
	cp stage2_eltorito iso/boot/grub/
	cp menu.lst iso/boot/grub/
	cp kernel.elf iso/boot/
	genisoimage -R                          \
		-b boot/grub/stage2_eltorito    \
		-no-emul-boot                   \
		-boot-load-size 4               \
		-A kernel                       \
		-input-charset utf8             \
		-quiet                          \
		-boot-info-table                \
		-o kernel.iso                   \
		iso

kernel.elf: loader.o link.ld
	ld -T link.ld -melf_i386 loader.o -o kernel.elf

loader.o: loader.s
	nasm -f elf32 loader.s

clean:
	rm -rf loader.o kernel.elf iso kernel.iso bochs.log
