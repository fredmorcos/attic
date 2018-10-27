#include <unistd.h>
// #include <errmsg.h>
#include <string.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

const char acstate[]="/proc/acpi/ac_adapter/C1BC/state";
char governor[]="/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor";
const char powersave[]="powersave";
const char performance[]="performance";
const char ondemand[]="ondemand";

main(int argc,char* argv[]) {
  char buf[1024];
  int fd,r;
  const char* s;
  // errmsg_iam("switch_governor");

  if ((s=strrchr(argv[0],'/')))
    ++s;
  else
    s=argv[0];
  if (!strcmp(s,"slow")) {
    s=powersave; goto writeit;
  } else if (!strcmp(s,"fast")) {
    s=performance; goto writeit;
  } else if (!strcmp(s,"auto")) {
    s=ondemand; goto writeit;
  }

  if ((fd=open(acstate,O_RDONLY))==-1)
  {
    // diesys(1,"could not open ",acstate);
    printf("could not open %s\n", acstate);
    exit(1);
  }
  if ((r=read(fd,buf,sizeof(buf)))<1)
  {
    // diesys(1,"read error");
    printf("read error\n");
    exit(1);
  }
  if (r>1000)
  {
    // die(1,"too much data in ",acstate);
    printf("too much data in %s\n", acstate);
    exit(1);
  }
  buf[r]=0;
  close(fd);
  if (strstr(buf,"off-line"))
    s=powersave;
  else if (strstr(buf,"on-line"))
    s=ondemand;
  else
  {
    // die(1,"ac adapter neither on-line nor off-line?!");
    printf("ac adapter neither on-line nor off-line?!\n");
    exit(1);
  }
writeit:
  {
    int i;
    char* x=strstr(governor,"cpu0");
    if (x) x+=3;
    for (i=0; i<10; ++i) {
      if ((fd=open(governor,O_WRONLY))==-1) {
	if (x && *x=='0')
	{
	  // diesys(1,"could not open ",governor);
	  printf("could not open %s\n", governor);
	  exit(1);
	}
	break;
      }
      if (write(fd,s,r=strlen(s))!=r)
      {
	// diesys(1,"write error");
	printf("write error\n");
	exit(1);
      }
      close(fd);
      if (x) ++(*x); else break;
    }
  }
  // carp("switched cpufreq governor to ",s);
  printf("switched cpufreq governor to %s\n", s);
}
