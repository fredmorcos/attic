/* --- UPCR system headers --- */ 
#include "upcr.h" 
#include "whirl2c.h"
#include "upcr_proxy.h"
/*******************************************************
 * C file translated from WHIRL Thu Sep 29 06:13:04 2011
 *******************************************************/

/* UPC Runtime specification expected: 3.6 */
#define UPCR_WANT_MAJOR 3
#define UPCR_WANT_MINOR 6
/* UPC translator version: release 2.12.2, built on May 15 2011 at 20:59:09, host aphid linux-x86_64/64, gcc v4.2.4 (Ubuntu 4.2.4-1ubuntu4) */
/* Included code from the initialization script */
#include "upcr_geninclude/stdio.h"
#include "upcr_geninclude/math.h"
#include</san1/home/gup/k339970/vector-addition/gettime.h>
#include "upcr_geninclude/stddef.h"
#include</home/ica/k339970/build/sandbox-upc-2.12.2/opt/include/upcr_preinclude/upc_bits.h>
#include "upcr_geninclude/stdlib.h"
#include "upcr_geninclude/inttypes.h"
#line 1 "main.w2c.h"
/* Include builtin types and operators */

#ifndef UPCR_TRANS_EXTRA_INCL
#define UPCR_TRANS_EXTRA_INCL
extern int upcrt_gcd (int _a, int _b);
extern int _upcrt_forall_start(int _start_thread, int _step, int _lo, int _scale);
#define upcrt_forall_start(start_thread, step, lo, scale)  \
       _upcrt_forall_start(start_thread, step, lo, scale)
int32_t UPCR_TLD_DEFINE_TENTATIVE(upcrt_forall_control, 4, 4);
#define upcr_forall_control upcrt_forall_control
#ifndef UPCR_EXIT_FUNCTION
#define UPCR_EXIT_FUNCTION() ((void)0)
#endif
#if UPCR_RUNTIME_SPEC_MAJOR > 3 || (UPCR_RUNTIME_SPEC_MAJOR == 3 && UPCR_RUNTIME_SPEC_MINOR >= 8)
  #define UPCRT_STARTUP_SHALLOC(sptr, blockbytes, numblocks, mult_by_threads, elemsz, typestr) \
      { &(sptr), (blockbytes), (numblocks), (mult_by_threads), (elemsz), #sptr, (typestr) }
#else
  #define UPCRT_STARTUP_SHALLOC(sptr, blockbytes, numblocks, mult_by_threads, elemsz, typestr) \
      { &(sptr), (blockbytes), (numblocks), (mult_by_threads) }
#endif
#define UPCRT_STARTUP_PSHALLOC UPCRT_STARTUP_SHALLOC

/**** Autonb optimization ********/

extern void _upcrt_memput_nb(upcr_shared_ptr_t _dst, const void *_src, size_t _n);
#define upcrt_memput_nb(dst, src, n) \
       (upcri_srcpos(), _upcrt_memput_nb(dst, src, n))

#endif


/* Types */
/* File-level vars and routines */
extern void print_vec(upcr_pshared_ptr_t);

extern int thread_id(int);

extern int user_main(int, char **);

extern void addvecs(_IEEE64 *, _IEEE64 *, _IEEE64 *, int, int);


#define UPCR_SHARED_SIZE_ 8
#define UPCR_PSHARED_SIZE_ 8
upcr_pshared_ptr_t veca;
upcr_pshared_ptr_t vecb;
upcr_pshared_ptr_t vecc;
upcr_pshared_ptr_t time_diffs;

void UPCRI_ALLOC_main_221775937(void) { 
UPCR_BEGIN_FUNCTION();
upcr_startup_pshalloc_t _bupc_pinfo[] = { 
UPCRT_STARTUP_PSHALLOC(veca, 8, 1000000, 0, 8, "A1000000_R1_d"), 
UPCRT_STARTUP_PSHALLOC(vecb, 8, 1000000, 0, 8, "A1000000_R1_d"), 
UPCRT_STARTUP_PSHALLOC(vecc, 8, 1000000, 0, 8, "A1000000_R1_d"), 
UPCRT_STARTUP_PSHALLOC(time_diffs, 8, 4, 0, 8, "A4_R1_d"), 
 };

UPCR_SET_SRCPOS("_main_221775937_ALLOC",0);
upcr_startup_pshalloc(_bupc_pinfo, sizeof(_bupc_pinfo) / sizeof(upcr_startup_pshalloc_t));
}

void UPCRI_INIT_main_221775937(void) { 
UPCR_BEGIN_FUNCTION();
UPCR_SET_SRCPOS("_main_221775937_INIT",0);
}

#line 29 "main.upc"
extern void print_vec(
  upcr_pshared_ptr_t vec)
#line 29 "main.upc"
{
#line 29 "main.upc"
  UPCR_BEGIN_FUNCTION();
  
  UPCR_EXIT_FUNCTION();
  return;
} /* print_vec */


#line 44 "main.upc"
extern int thread_id(
  int i)
#line 44 "main.upc"
{
#line 44 "main.upc"
  UPCR_BEGIN_FUNCTION();
  
#line 45 "main.upc"
  UPCR_EXIT_FUNCTION();
#line 45 "main.upc"
  return i;
} /* thread_id */


#line 49 "main.upc"
extern int user_main(
  int argc,
  char ** argv)
#line 49 "main.upc"
{
#line 49 "main.upc"
  UPCR_BEGIN_FUNCTION();
  register _IEEE64 _bupc_comma;
  int i;
  int run;
  struct timespec start;
  _IEEE64 vecblock_a[1LL];
  _IEEE64 vecblock_b[1LL];
  _IEEE64 vecblock_c[1LL];
  struct timespec end;
  _IEEE64 _bupc___save_expr0;
  int thread;
  upcr_pshared_ptr_t _bupc_Mptra0;
  _IEEE64 _bupc_spillstoreparm1;
  upcr_pshared_ptr_t _bupc_Mptra2;
  _IEEE64 _bupc_spillld3;
  upcr_pshared_ptr_t _bupc_Mptra4;
  upcr_pshared_ptr_t _bupc_Mptra5;
  upcr_shared_ptr_t _bupc_Mstopcvt6;
  upcr_pshared_ptr_t _bupc_Mptra7;
  upcr_shared_ptr_t _bupc_Mstopcvt8;
  upcr_pshared_ptr_t _bupc_Mptra9;
  upcr_shared_ptr_t _bupc_Mstopcvt10;
  upcr_pshared_ptr_t _bupc_Mptra11;
  upcr_pshared_ptr_t _bupc_Mptra12;
  _IEEE64 _bupc_spillld13;
  upcr_pshared_ptr_t _bupc_Mptra14;
  _IEEE64 _bupc_spillld15;
  
#line 58 "main.upc"
  if(((int) upcr_mythread () ) == 0)
#line 58 "main.upc"
  {
#line 59 "main.upc"
    i = 0;
#line 59 "main.upc"
    while(i <= 999999)
#line 59 "main.upc"
    {
#line 60 "main.upc"
      _bupc_Mptra0 = UPCR_ADD_PSHARED1(vecb, 8ULL, i);
      _bupc_spillstoreparm1 = (_IEEE64)(i);
#line 60 "main.upc"
      UPCR_PUT_PSHARED(_bupc_Mptra0, 0, &_bupc_spillstoreparm1, 8);
#line 60 "main.upc"
      _bupc_Mptra4 = UPCR_ADD_PSHARED1(veca, 8ULL, i);
#line 60 "main.upc"
      _bupc_Mptra2 = UPCR_ADD_PSHARED1(vecb, 8ULL, i);
#line 60 "main.upc"
      _bupc_spillld3 = UPCR_GET_PSHARED_DOUBLEVAL(_bupc_Mptra2, 0);
#line 60 "main.upc"
      UPCR_PUT_PSHARED(_bupc_Mptra4, 0, &_bupc_spillld3, 8);
#line 60 "main.upc"
      _1 :;
#line 60 "main.upc"
      i = i + 1;
    }
#line 62 "main.upc"
    print_vec(veca);
#line 63 "main.upc"
    print_vec(vecb);
  }
#line 66 "main.upc"
  upcr_barrier(221775937, 1);
#line 68 "main.upc"
  run = 0;
#line 68 "main.upc"
  while(run <= 2)
#line 68 "main.upc"
  {
#line 69 "main.upc"
    get_time_nsec(&start);
#line 139 "main.upc"
    if(((int) upcr_mythread () ) == 0)
#line 139 "main.upc"
    {
#line 142 "main.upc"
      i = 0;
#line 142 "main.upc"
      while(i <= 999999)
#line 142 "main.upc"
      {
#line 143 "main.upc"
        _bupc_Mptra5 = UPCR_ADD_PSHARED1(veca, 8ULL, i);
#line 143 "main.upc"
        _bupc_Mstopcvt6 = UPCR_PSHARED_TO_SHARED(_bupc_Mptra5);
#line 143 "main.upc"
        upc_memget(vecblock_a, _bupc_Mstopcvt6, (unsigned long) 8ULL);
#line 144 "main.upc"
        _bupc_Mptra7 = UPCR_ADD_PSHARED1(vecb, 8ULL, i);
#line 144 "main.upc"
        _bupc_Mstopcvt8 = UPCR_PSHARED_TO_SHARED(_bupc_Mptra7);
#line 144 "main.upc"
        upc_memget(vecblock_b, _bupc_Mstopcvt8, (unsigned long) 8ULL);
#line 146 "main.upc"
        addvecs(vecblock_a, vecblock_b, vecblock_c, (int) 0, (int) 1);
#line 148 "main.upc"
        _bupc_Mptra9 = UPCR_ADD_PSHARED1(vecc, 8ULL, i);
#line 148 "main.upc"
        _bupc_Mstopcvt10 = UPCR_PSHARED_TO_SHARED(_bupc_Mptra9);
#line 148 "main.upc"
        upc_memput(_bupc_Mstopcvt10, vecblock_c, (unsigned long) 8ULL);
#line 149 "main.upc"
        _3 :;
#line 149 "main.upc"
        i = i + 1;
      }
    }
#line 192 "main.upc"
    get_time_nsec(&end);
#line 193 "main.upc"
    _bupc_comma = time_diff_nsec(&start, &end);
#line 193 "main.upc"
    _bupc_Mptra11 = UPCR_ADD_PSHARED1(time_diffs, 8ULL, ((int) upcr_mythread () ));
    UPCR_PUT_PSHARED_DOUBLEVAL(_bupc_Mptra11, 0, _bupc_comma);
#line 194 "main.upc"
    upcr_barrier(221775938, 1);
#line 195 "main.upc"
    _2 :;
#line 195 "main.upc"
    run = run + 1;
  }
#line 197 "main.upc"
  upcr_barrier(221775939, 1);
#line 199 "main.upc"
  if(((int) upcr_mythread () ) == 0)
#line 199 "main.upc"
  {
#line 200 "main.upc"
    print_vec(vecc);
#line 203 "main.upc"
    i = 0;
#line 203 "main.upc"
    while(i <= 999999)
#line 203 "main.upc"
    {
#line 204 "main.upc"
      _bupc___save_expr0 = (_IEEE64)(i);
#line 204 "main.upc"
      _bupc_Mptra12 = UPCR_ADD_PSHARED1(vecc, 8ULL, i);
#line 204 "main.upc"
      _bupc_spillld13 = UPCR_GET_PSHARED_DOUBLEVAL(_bupc_Mptra12, 0);
#line 204 "main.upc"
      if(_bupc_spillld13 != (_bupc___save_expr0 + _bupc___save_expr0))
#line 204 "main.upc"
      {
#line 205 "main.upc"
        printf("error\n");
#line 206 "main.upc"
        goto _5;
      }
#line 208 "main.upc"
      _4 :;
#line 208 "main.upc"
      i = i + 1;
    }
#line 208 "main.upc"
    printf("success\n");
#line 211 "main.upc"
    _5 :;
  }
#line 213 "main.upc"
  if(((int) upcr_mythread () ) == 0)
#line 213 "main.upc"
  {
#line 214 "main.upc"
    thread = 0;
#line 214 "main.upc"
    while(thread <= 3)
#line 214 "main.upc"
    {
#line 215 "main.upc"
      _bupc_Mptra14 = UPCR_ADD_PSHARED1(time_diffs, 8ULL, thread);
#line 215 "main.upc"
      _bupc_spillld15 = UPCR_GET_PSHARED_DOUBLEVAL(_bupc_Mptra14, 0);
#line 215 "main.upc"
      printf("T %2d: %.15f s\n", thread, _bupc_spillld15);
#line 215 "main.upc"
      _6 :;
#line 215 "main.upc"
      thread = thread + 1;
    }
  }
#line 217 "main.upc"
  UPCR_EXIT_FUNCTION();
#line 217 "main.upc"
  return 0;
} /* user_main */

#line 1 "_SYSTEM"
/**************************************************************************/
/* upcc-generated strings for configuration consistency checks            */

GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_GASNetConfig_gen, 
 "$GASNetConfig: (/tmp/upcc--30575-1317301984/main.trans.c) RELEASE=1.16.2,SPEC=1.8,CONDUIT=IBV(IBV-1.13/IBV-1.12),THREADMODEL=SEQ,SEGMENT=FAST,PTR=64bit,noalign,nopshm,nodebug,notrace,nostats,nosrclines,timers_native,membars_native,atomics_native,atomic32_native,atomic64_native $");
GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_UPCRConfig_gen,
 "$UPCRConfig: (/tmp/upcc--30575-1317301984/main.trans.c) VERSION=2.12.2,PLATFORMENV=distributed,SHMEM=none,SHAREDPTRREP=packed/p21:t8:a35,TRANS=berkeleyupc,nodebug,nogasp,notv,staticthreads=4 $");
GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_translatetime, 
 "$UPCTranslateTime: (main.o) Thu Sep 29 06:13:04 2011 $");
GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_GASNetConfig_obj, 
 "$GASNetConfig: (main.o) " GASNET_CONFIG_STRING " $");
GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_UPCRConfig_obj,
 "$UPCRConfig: (main.o) " UPCR_CONFIG_STRING UPCRI_THREADCONFIG_STR " $");
GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_translator, 
 "$UPCTranslator: (main.o) /usr/local/upc/2.12.2/translator/install/targ (aphid) $");
GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_upcver, 
 "$UPCVersion: (main.o) 2.12.2 $");
GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_compileline, 
 "$UPCCompileLine: (main.o) /usr/local/upc/2.12.2/runtime/inst/bin/upcc.pl --at-remote-http --arch-size=64 --network=ibv -T 4 --lines --trans --sizes-file=upcc-sizes main.i $");
GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_compiletime, 
 "$UPCCompileTime: (main.o) " __DATE__ " " __TIME__ " $");
#ifndef UPCRI_CC /* ensure backward compatilibity for http netcompile */
#define UPCRI_CC <unknown>
#endif
GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_backendcompiler, 
 "$UPCRBackendCompiler: (main.o) " _STRINGIFY(UPCRI_CC) " $");

#ifdef GASNETT_CONFIGURE_MISMATCH
  GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_configuremismatch, 
   "$UPCRConfigureMismatch: (main.o) 1 $");
  GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_configuredcompiler, 
   "$UPCRConfigureCompiler: (main.o) " GASNETT_PLATFORM_COMPILER_IDSTR " $");
  GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_buildcompiler, 
   "$UPCRBuildCompiler: (main.o) " PLATFORM_COMPILER_IDSTR " $");
#endif

/**************************************************************************/
GASNETT_IDENT(UPCRI_IdentString_main_o_1317301984_transver_2,
 "$UPCTranslatorVersion: (main.o) 2.12.2, built on May 15 2011 at 20:59:09, host aphid linux-x86_64/64, gcc v4.2.4 (Ubuntu 4.2.4-1ubuntu4) $");
