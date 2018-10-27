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

module Extra;

alias char[] string;

enum TokenType {
	WS,		/* whitespace */
	ID,		/* identifier */
	SL,		/* string literal */
	NM,		/* integer literal */
	KW,		/* keyword */
	LKW,	/* let keyword */
	LRKW,	/* letrec keyword */
	FKW,	/* func keyword */
	IKW,	/* if keyword */
	EKW,	/* else keyword */
	INKW,	/* in keyword */
	BF,		/* built-in */
	NBF,	/* the "not" built-in */
	SBF,	/* getStr built-in */
	IBF,	/* getInt built-in */
	PBF,	/* print built-in */
	OT,		/* other */
	SM, 	/* semi-colon */
	PO,		/* addition operator */
	MO,		/* subtraction operator */
	MB,		/* multiplication operator */
	DB,		/* division operator */
	AO,		/* assign operator */
	EQ,		/* equal to operator */
	FA,		/* comma operator */
	GT,		/* greater than operator */
	LT,		/* less than operator */
	LC,		/* left curly operator */
	RC,		/* right curly operator */
	LB,		/* left parenthesis operator */
	RB,		/* right parenthesis operator */
	CA,		/* and (double ampersand) operator */
	CO		/* or (double pipe) operator */
};

static string[]
	keywords = ["let", "letrec", "func", "if", "else", "in"],
	builtins = ["not", "getStr", "getInt", "print"];

bool isKeyword (ref string c) {
	for (int i = 0; i < keywords.length; i++)
		if (c == keywords[i])
			return true;
	return false;
}

TokenType typeFromStr (ref string tok) {
	if (tok == "let")
		return TokenType.LKW;
	else if (tok == "letrec")
		return TokenType.LRKW;
	else if (tok == "func")
		return TokenType.FKW;
	else if (tok == "if")
		return TokenType.IKW;
	else if (tok == "else")
		return TokenType.EKW;
	else if (tok == "in")
		return TokenType.INKW;
	else if (tok == "not")
		return TokenType.NBF;
	else if (tok == "getStr")
		return TokenType.SBF;
	else if (tok == "getInt")
		return TokenType.IBF;
	else if (tok == "print")
		return TokenType.PBF;
	else
		return TokenType.OT;
}
				
bool isBuiltin (ref string c) {
	for (int i = 0; i < builtins.length; i++)
		if (c == builtins[i])
			return true;
	return false;
}

bool isOperator(char c) {
	return (c=='+'||c=='-'||c=='*'||c=='/'||c=='='||c==','||c=='>'||c=='<'||
			c=='{'||c=='}'||c=='('||c==')'||c=='&'||c=='|'||c==';')?true:false;
}

TokenType typeFromOp (string op) {
	if (op == "+")			return TokenType.PO;
	else if (op == "-")		return TokenType.MO;
	else if (op == "*")		return TokenType.MB;
	else if (op == "/")		return TokenType.DB;
	else if (op == "=")		return TokenType.AO;
	else if (op == "==")	return TokenType.EQ;
	else if (op == ",")		return TokenType.FA;
	else if (op == ">")		return TokenType.GT;
	else if (op == "<")		return TokenType.LT;
	else if (op == "{")		return TokenType.LC;
	else if (op == "}")		return TokenType.RC;
	else if (op == "(")		return TokenType.LB;
	else if (op == ")")		return TokenType.RB;
	else if (op == "&&")	return TokenType.CA;
	else if (op == "||")	return TokenType.CO;
	else if (op == ";")		return TokenType.SM;
	else 					return TokenType.OT;
}

string stringFromType (TokenType t) {
	switch (t) {
		case TokenType.WS:		return "WS";
		case TokenType.ID:		return "ID";
		case TokenType.SL:		return "SL";
		case TokenType.NM:		return "NM";
		case TokenType.KW:		return "KW";
		case TokenType.LKW:		return "LKW";
		case TokenType.LRKW:	return "LRKW";
		case TokenType.FKW:		return "FKW";
		case TokenType.IKW:		return "IKW";
		case TokenType.EKW:		return "EKW";
		case TokenType.INKW:	return "INKW";
		case TokenType.BF:		return "BF";
		case TokenType.NBF:		return "NBF";
		case TokenType.SBF:		return "SBF";
		case TokenType.IBF:		return "IBF";
		case TokenType.PBF:		return "PBF";
		case TokenType.SM:		return "SM";
		case TokenType.PO:		return "PO";
		case TokenType.MO: 		return "MO";
		case TokenType.MB: 		return "MB";
		case TokenType.DB: 		return "DB";
		case TokenType.AO: 		return "AO";
		case TokenType.EQ:		return "EQ";
		case TokenType.FA:		return "FA";
		case TokenType.GT:		return "GT";
		case TokenType.LT:		return "LT";
		case TokenType.LC:		return "LC";
		case TokenType.RC:		return "RC";
		case TokenType.LB:		return "LB";
		case TokenType.RB:		return "RB";
		case TokenType.CA:		return "CA";
		case TokenType.CO:		return "CO";
		case TokenType.OT:		return "OT";
		default:				return "Unknown";
	}
}

