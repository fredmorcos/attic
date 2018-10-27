#include <stdint.h>

void kmain(void)
{
   extern uint32_t magic;
   extern void *mbd;

   if (magic != 0x2BADB002)
     {
       /* something went wrong, cannot depend on bootloader magic numbeer */
     }
 
   /* You could either use multiboot.h */
   /* (http://www.gnu.org/software/grub/manual/multiboot/multiboot.html#multiboot_002eh) */
   /* or do your offsets yourself. The following is merely an example. */ 
   char * boot_loader_name =(char*) ((long*)mbd)[16];
 
   /* Print a letter to screen to see everything is working: */
   unsigned char *videoram = (unsigned char *)0xB8000;
   videoram[0] = 65; /* character 'A' */
   videoram[1] = 0x07; /* light grey (7) on black (0). */
   videoram[2] = boot_loader_name[0];
}
