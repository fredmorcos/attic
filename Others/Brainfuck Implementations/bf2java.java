/*
 *  BF2Java
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
 * Program Name:  BF2Java
 * Version:       1.0
 * Date:          2003-03-18
 * Description:   Converts Brainfuck source to Java source
 * License:       GPL
 * Web page:      http://www.brainfuck.ca
 * Download:      http://www.brainfuck.ca/BF2Java.java
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
 * Related Progs: BF2C
 * Category:      Software Development > Programming language conversion
 */

import java.io.BufferedReader;
import java.io.FileReader;

/**
 *  BF2Java - convert Brainfuck to Java
 *  @author Thomas Cort (<A HREF="mailto:tom@tomcort.com">tom@tomcort.com</A>)
 *  @version 1.1 2003-03-16
 *  @since   1.0
 */
public class BF2Java {

  /**
   *  Converts Brainfuck to Java
   *  @param  String - BF  code
   *  @return String - Java code
   *  @since 1.0
   */
  public static String convert(String s) {

String javaCode = "" +
"import java.io.BufferedReader;\n" +
"import java.io.InputStreamReader;\n" +
"\n" +
"public class output {\n" +
"\n" +
"  public static void putchar(byte c) {\n" +
"    byte [] out = {c};\n" +
"    String s = new String(out);" +
"    System.out.print(s);" +
"  }" +
"\n" +
"public static byte getchar() {\n" +
"  BufferedReader Stream = \n" +
"    new BufferedReader(new InputStreamReader(System.in));\n" +
"\n" +
"  try {\n" +
"    String str = Stream.readLine();\n" +
"    byte [] in = str.getBytes();\n" +
"    return in[0];\n" +
"  } catch (Exception e) {\n" +
"    throw new Error(\"Input Parse Error\");\n" +
"  }\n" +
"}\n" +
"\n" +
"public static void main(String [] args) {\n"+
"  int pc = 0;\n" +
"  byte[] x = new byte[32768];\n";

    for(int i = 0; i < s.length(); i++) {
      if      (s.charAt(i) == '>') javaCode += "    pc++;\n";
      else if (s.charAt(i) == '<') javaCode += "    pc--;\n";
      else if (s.charAt(i) == '+') javaCode += "    x[pc]++;\n";
      else if (s.charAt(i) == '-') javaCode += "    x[pc]--;\n";
      else if (s.charAt(i) == '.') javaCode += "    putchar(x[pc]);\n";
      else if (s.charAt(i) == ',') javaCode += "    x[pc] = getchar();\n";
      else if (s.charAt(i) == '[') javaCode += "    while (x[pc] != 0) {\n";
      else if (s.charAt(i) == ']') javaCode += "    }\n";
    }

    return javaCode + "}\n}";
  }

  /**
   *  Error Handling
   *  @param String - error message
   *  @since 1.0
   */
  public static void errBF2Java(String s) {
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
        errBF2Java("Cannot read input file");
      }

      System.out.println(BF2Java.convert(input));
    }
  }

}

