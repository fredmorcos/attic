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

module Parser;

private import
	Lexer,
	Token,
	Extra,
	tango.io.Stdout,
	tango.io.device.File,
	Integer = tango.text.convert.Integer;

class ParserException: Exception {
public:
	this () {
		super ("Parser Exception.");
	}
}

class ParserFinishedException: Exception {
public:
	this () {
		super ("Parser Finished Exception.");
	}
}

class Parser {
public:
	this (string filename) {
		lexer = new Lexer(filename);
	}
	
	void debugToken(string fname) {
/*		while (fname.length < 15)
			fname ~= " ";
			
		if (token is null) {
			Stdout.formatln("Token is null!");
			return;
		}
			
		Stdout.formatln("{}\t\t{}\t\t\t{}", fname, token.lexeme, 
						stringFromType(token.type));
*/	}
	
	void run() {
		Token[] tokens = lexer.run;
		string	output;
		int		caseCount = 0;
		
		// Validation with XTokenizer.in - DONE
//		for (int i = 0; i < tokens.length; i++)
//			Stdout.formatln("[{}] {}", stringFromType(tokens[i].type), 
//									   tokens[i].lexeme);
		
		while ((token = lexer.nextToken) !is null)
			Stdout.formatln("[{}] {}", stringFromType(token.type), 
									   token.lexeme);
		
/*		token = lexer.nextToken;
		while (token !is null) {
			debugToken("run");
			
			// Stdout.formatln("[Case {}]", ++caseCount);
			output ~= "[Case " ~ Integer.toString(++caseCount) ~ "]\n";
			try {
				program;
			//	Stdout("Yes!").newline.newline;
				output ~= "Yes!\n\n";
			}
			catch (ParserException e) {
			//	Stdout("No!").newline.newline;
				output ~= "No!\n\n";
				break;
			}
			catch (ParserFinishedException e) {
			//	Stdout("Yes!").newline.newline;
				output ~= "Yes!\n\n";
				break;
			}
		}
		
		File.set("output.txt", output);
*/	}
	
	void program() {
		debugToken("program");
		
		expression;
		match(TokenType.SM);
	}

	void expression() {
		debugToken("expression");

		comparison;
	
		while(token.type == TokenType.CA || token.type == TokenType.CO) {
			match(token.type);
			comparison;
		}
	}

	void comparison() {
		debugToken("comparison");
		
		arithmetic;
	
		while(token.type == TokenType.EQ || token.type == TokenType.GT || 
			  token.type == TokenType.LT) {
			match(token.type);
			arithmetic;
		}
	}

	void arithmetic() {
		debugToken("arithmetic");
		
		term;
	
		while(token.type == TokenType.PO || token.type == TokenType.MO) {
			match(token.type);
			term;
		}
	}

	void term() {
		debugToken("term");
		
		primary;
	
		while(token.type == TokenType.MB || token.type == TokenType.DB) {
			match(token.type);
			primary;
		}
	}

	void primary() {
		debugToken("primary");
		
		notfunccall;
	
		while(token.type == TokenType.LB)
			arguments;
	}

	void notfunccall() {
		debugToken("notfunccall");
	
		if(token.type == TokenType.LB) {
			match(TokenType.LB);
			expression;
			match(TokenType.RB);
		}
		else if(token.type == TokenType.KW && token.lexeme == "let") {
			matchText(TokenType.KW, "let");
			bindings;
			matchText(TokenType.KW, "in");
			expression;
		}
		else if(token.type == TokenType.KW && token.lexeme == "letrec") {
			matchText(TokenType.KW, "letrec");
			bindings;
			matchText(TokenType.KW, "in");
			expression;
		}
		else if(token.type == TokenType.KW && token.lexeme == "func") {
			matchText(TokenType.KW, "func");
			parameters;
			match(TokenType.LC);
			expression;
			match(TokenType.RC);
		}
		else if(token.type == TokenType.KW && token.lexeme == "if") {
			matchText(TokenType.KW, "if");
			match(TokenType.LB);
			expression;
			match(TokenType.RB);
			match(TokenType.LC);
			expression;
			match(TokenType.RC);
			matchText(TokenType.KW, "else");
			match(TokenType.LC);
			expression;
			match(TokenType.RC);
		}
		else if(token.type == TokenType.ID)
			match(token.type);
		else if (token.type == TokenType.BF)
			match(token.type);
		else if(token.type == TokenType.SL)
			match(token.type);
		else if(token.type == TokenType.NM)
			match(token.type);
	}

	void bindings() {
		debugToken("bindings");
	
		match(TokenType.ID);
		match(TokenType.AO);
		expression;
	
		while(token.type == TokenType.FA) {
			match(TokenType.FA);
			match(TokenType.ID);
			match(TokenType.AO);
			expression;
		}	
	}

	void parameters() {
		debugToken("parameters");
		
		match(TokenType.LB);
		if(token.type != TokenType.RB) {
			match(TokenType.ID);
		
			while(token.type == TokenType.FA) {
				match(TokenType.FA);
				match(TokenType.ID);
			}
		}
		match(TokenType.RB);
	}

	void arguments() {
		debugToken("arguments");
		
		match(TokenType.LB);
		if(token.type != TokenType.RB) {
			expression;
		
			while(token.type == TokenType.FA) {
				match(TokenType.FA);
				expression;
			}
		}
		match(TokenType.RB);
	}

	void match(TokenType type) {
		debugToken("match");
		
		if(token.type == type) {
			token = lexer.nextToken;
			if (token is null)
				throw new ParserFinishedException();
		}
		else
			throw new ParserException();
			
		debugToken("after-match");
	}

	void matchText(TokenType type, string text) {
		debugToken("matchText");
		
		if(token.type == type && token.lexeme == text) {
			token = lexer.nextToken;
			if (token is null)
				throw new ParserFinishedException();
		}
		else
			throw new ParserException();
			
		debugToken("after-matchText");
	}
	
protected:
	Lexer	lexer;
	Token	token;
}

