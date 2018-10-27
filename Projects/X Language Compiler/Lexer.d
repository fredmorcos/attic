/*	
 *	This file is part of xc.
 *
 *	Copyright 2009	Frederic Morcos <fred.morcos@gmail.com>
 *
 *	xc is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	xc is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with xc.  If not, see <http://www.gnu.org/licenses/>.
 */

module Lexer;

private import
	Extra,
	Token,
	tango.io.device.File,
	tango.stdc.ctype;

class Lexer {
public:
	this (string filename) {
		data = cast(string) File.get(filename);
	}

	Token nextToken () {
		static int count = 0;
		
		if (count == tokens.length)
			return null;
		
		return tokens[count++];
	}
	
	Token[] run () {
		string	lexeme;
		char	c;
		tokens = [];
	
		for (int i = 0; i < data.length; i++) {
			c = data[i];
			lexeme = "";
			if (isspace(c) || c == ' ' || c == '\b') {
				while (i < data.length - 1) {
					c = data[++i];
					
					if (!isspace(c) || c == ' ' || c == '\b') {
						--i;
						break;
					}
				}
			}
			else if (c == '"') {
				bool invalid = false;
				lexeme ~= c;
				
				while (1) {
					c = data[++i];
					if (c == '\n' || c == '\r' || i == data.length) {
						--i;
						invalid = true;
						break;
					}
					else if (c == '"') {
						lexeme ~= c;
						break;
					}
					else {
						lexeme ~= c;
					}
				}
			
				if (!invalid)
					tokens ~= new Token (lexeme, TokenType.SL, 
										 "Invalid string literal.", true);
				else
					tokens ~= new Token (lexeme, TokenType.SL);
			}
			else if (isOperator(c)) {
				lexeme ~= c;
				if (c == '=')
					if ((c = data[++i]) == '=')
						lexeme ~= c;
					else
						--i;
				else if (c == '&')
					if ((c = data[++i]) == '&')
						lexeme ~= c;
					else
						--i;
				else if (c == '|')
					if ((c = data[++i]) == '|')
						lexeme ~= c;
					else
						--i;

				TokenType t = typeFromOp(lexeme);
				
				if (t == TokenType.OT)
					tokens ~= new Token (lexeme, t, "Invalid operator.", true);
				else
					tokens ~= new Token (lexeme, t);
			}
			else if (isdigit(c)) {
				lexeme ~= c;
				while (i < data.length - 1) {
					c = data[++i];
					
					if (!isdigit(c)) {
						--i;
						break;
					}
					else
						lexeme ~= c;
				}

				tokens ~= new Token (lexeme, TokenType.NM);
			}
			else if (isalpha(c) || c == '_') {
				lexeme ~= c;
				while (i < data.length - 1) {
					c = data[++i];
					
					if (isalpha(c) || c == '_' || isdigit(c))
						lexeme ~= c;
					else {
						--i;
						break;
					}
				}
			
				if (isKeyword(lexeme))
					tokens ~= new Token (lexeme, typeFromStr(lexeme));
				else if (isBuiltin(lexeme))
					tokens ~= new Token (lexeme, typeFromStr(lexeme));
				else
					tokens ~= new Token (lexeme, TokenType.ID);
			}
			else {
				lexeme ~= c;
				tokens ~= new Token (lexeme, TokenType.OT, 
									 "Something strange.", true);
			}
		}
		
		return tokens;
	}
	
protected:
	string	data;
	Token[]	tokens;
}

