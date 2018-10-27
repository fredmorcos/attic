/*
 *  BF2C
 *  Copyright (C) 2003 Thomas Cort
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 */

/*
 * Program Name:  BF2C
 * Version:       1.0
 * Date:          2003-03-18
 * Description:   Converts Brainfuck source to C source
 * License:       GPL
 * Web page:      http://www.brainfuck.ca
 * Download:      http://www.brainfuck.ca/BF2C.java
 * Source Info:   http://www.brainfuck.ca/downloads.html
 * Latest Ver:    http://www.brainfuck.ca/downloads.html
 * Documentation: None
 * Help:          tom@brainfuck.ca
 * Developement:  tom@brainfuck.ca
 * Bugs:          tom@brainfuck.ca
 * Maintainer:    Thomas Cort <tom@brainfuck.ca>
 * Developer:     Thomas Cort <tom@brainfuck.ca>
 * Interfaces:    Command Line
 * Source Lang:   Java
 * Build Prereq:  None
 * Related Progs: BF2Java
 * Category:      Software Development > Programming language conversion
 */

import java.io.BufferedReader;
import java.io.FileReader;

/**
 *  BF2C - convert Brainfuck to C
 *  @author Thomas Cort (<A HREF="mailto:tom@tomcort.com">tom@tomcort.com</A>)
 *  @version 1.1 2003-03-16
 *  @since   1.0
 */
public class BF2C {

  /**
   *  Converts Brainfuck to C
   *  @param  String - BF code
   *  @return String - C  code
   *  @since 1.0
   */
  public static String convert(String s) {

    String cCode = "#include <stdio.h>\nint main() {\n  int pc =0;\n" +
                   "  int x[32768];\n  for (;pc < 32768; pc++)\n"     +
                   "  x[pc] = 0;\n  pc = 0;\n";


    for(int i = 0; i < s.length(); i++) {
      if      (s.charAt(i) == '>') cCode += "  pc++;\n";
      else if (s.charAt(i) == '<') cCode += "  pc--;\n";
      else if (s.charAt(i) == '+') cCode += "  x[pc]++;\n";
      else if (s.charAt(i) == '-') cCode += "  x[pc]--;\n";
      else if (s.charAt(i) == '.') cCode += "  putchar(x[pc]);\n";
      else if (s.charAt(i) == ',') cCode += "  x[pc] = getchar();\n";
      else if (s.charAt(i) == '[') cCode += "  while (x[pc] != 0) {\n";
      else if (s.charAt(i) == ']') cCode += "  }\n";
    }

    return cCode + "  return 0;\n}";
  }

  /**
   *  Error Handling
   *  @param String - error message
   *  @since 1.0
   */
  public static void errBF2C(String s) {
    throw new Error(s);
  }

  public static void main(String [] args) throws Exception {

    BufferedReader reader;
    String line = "", input = "", filename = "";

    // Read each file and interpret.
    for(int z = 0; z < args.length; z++) {
      filename = args[z];
      input = "";
      
      try {
        reader = new BufferedReader( new FileReader( filename ) );
        while ( (line = reader.readLine()) != null )
          input += line;
      } catch (Exception e) {
        errBF2C("Cannot read input file");
      }

      System.out.println(BF2C.convert(input));
    }
  }

}
