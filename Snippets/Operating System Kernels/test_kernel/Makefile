AS = nasm
ASFLAGS = -f elf64
LD = ld
LDFLAGS = -T linker.ld
CC = gcc
CCFLAGS = -Wall -Wextra -Werror -nostdlib -fno-builtin \
	  -nostartfiles -nodefaultlibs -ffreestanding

.PHONY: default emulate clean
default: emulate

emulate: disk.img
	qemu-system-x86_64 -hda disk.img

disk.img: kernel.bin syslinux.cfg
	fallocate -l 33554432 disk.img
	mkfs.vfat disk.img
	syslinux -i disk.img
	mkdir mounted
	sudo mount disk.img mounted
	sudo cp syslinux.cfg mounted/.
	sudo cp kernel.bin mounted/.
	sync
	sudo umount mounted
	rmdir mounted

kernel.bin: loader.o linker.ld kernel.o
	$(LD) $(LDFLAGS) -o kernel.bin loader.o kernel.o

loader.o: loader_nasm.s
	$(AS) $(ASFLAGS) -o loader.o loader_nasm.s

kernel.o: kernel.c
	$(CC) $(CCFLAGS) -o kernel.o -c kernel.c

clean:
	rm -rf *.o *~ disk.img kernel.bin
