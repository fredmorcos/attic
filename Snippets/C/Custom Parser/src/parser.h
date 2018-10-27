#ifndef __PARSER_H__
#define __PARSER_H__

#include <stdio.h>

#include "ast.h"

#define PPROGRAM		0
#define PFUNCTION		1
#define PARGUMENT		2
#define PPARAMETER		3
#define PSTATEMENT		4
#define POPERATOR		5
#define POPERAND		6
#define PFUNCALL		7
#define PDECLARATION	8
#define PMODIFIER		9
#define PLITERAL		10
#define PIDENTIFIER		11

Ast *parser_build_ast(FILE *);

#endif /* __PARSER_H__ */

