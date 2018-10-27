/* BrainFuck compiler.  Copyright 1997 Ben Olmstead.  This program is
/* governed by the GNU public license.  See the file COPYING for details.
/*
/* This compiler generates native machine code for whatever processor.
/* It is currently configured to generate an MS-DOS .COM file, but it
/* should be relatively easy to alter.
/************************************************************************/
#include <stdio.h>
/************************************************************************/
void compile();
/************************************************************************/
unsigned short location = 3;
unsigned short datasize = 1;
unsigned short ptr = 1;
FILE *z, *Z;
char b;
unsigned short n;
/************************************************************************/
main( int argc, char *argv[] )
{
  if ( z = fopen( argv[1], "r" ) )
  {
    if ( Z = fopen( "a.com", "wb" ) )
    {
      fwrite( "\xbe\x00\x00", 1, 3, Z );
      compile();
      fwrite( "\xb8\x00\x4c\xcd\x21", 1, 5, Z );
      location += 261;
      for ( ptr = 0; ptr < datasize; ptr++ )
      {
        fwrite( "\x00", 1, 1, Z );
      }
      fseek( Z, 1, SEEK_SET );
      fwrite( &location, 1, 2, Z );
      fclose( Z );
    }
    fclose( z );
  }
}
/************************************************************************/
void compile()
{
  unsigned short m;
  while( ( b = getc( z ) ) != EOF )
  {
    switch ( b )
    {
      case '<': fwrite( "\x4e", 1, 1, Z ); location++; ptr--; break;
      case '>':
        fwrite( "\x46", 1, 1, Z );
        location++;
        ptr++;
        if ( ptr > datasize ) datasize = ptr;
        break;
      case '+': fwrite( "\xfe\x04", 1, 2, Z ); location += 2; break;
      case '-': fwrite( "\xfe\x0c", 1, 2, Z ); location += 2; break;
      case '.':
        fwrite( "\xb4\x40\xbb\x01\x00\x89\xd9\x89\xf2\xcd\x21", 1, 11, Z );
        location += 11;
        break;
      case ',':
        fwrite( "\xb4\x01\xcd\x21\x3c\x0d\x75\x07\xb8\x0a\x02\xb2\x0a\xcd\x21\x88\x04", 1, 17, Z );
        location += 17;
        break;
      case '[':
        m = location;
        fwrite( "\x80\x3c\x00\x75\x03\xe9\x00\x00", 1, 8, Z );
        location += 8;
        compile();
        fwrite( "\xe9", 1, 1, Z );
        location += 3;
        n = m - location;
        fwrite( &n, 1, 2, Z );
        fseek( Z, m + 6, SEEK_SET );
        n = location - m - 8;
        fwrite( &n, 1, 2, Z );
        fseek( Z, 0, SEEK_END );
        break;
      case ']': return;
    }
  }
}

