%{
/*******************************************************
Author:LHA
Date:10.6
*******************************************************/
#include "symboltable.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
char idStr[50];
int yylex();
extern int yyparse();
FILE* yyin;
void yyerror(const char* s );
%}

%union {
	double dval;
	struct symtab *symp;
}
%token <symp> NAME
%token <dval> NUMBER

%left '+' '-'
%left '*' '/'
%right UMINUS

%type <dval> expression

%%

lines	:	lines statement ';'
		|	lines ';'
		|
		;

statement	:	NAME '=' expression	{ $1->value = $3; }
			|	expression	{ printf("%g\n", $1); }
			;

expression	:	expression '+' expression	{ $$ = $1 + $3; }
			|	expression '-' expression	{ $$ = $1 - $3; }
			|	expression '*' expression	{ $$ = $1 * $3; }
			|	expression '/' expression	
							{	if ($3 == 0.0)
									yyerror("divide by zero");
								else
									$$ = $1 / $3;
							}
			|	'-' expression %prec UMINUS	{ $$ = -$2; }
			|	'(' expression ')'	{ $$ = $2; }
			|	NUMBER
			|	NAME	{ $$ = $1->value; }
			;

%%

struct symtab* symlook(char *s) {
	struct symtab *sp;
	for (sp = symtable; sp < &symtable[100]; sp++) {
		//若在sp在符号表中，返回sp地址，通过地址取值
		if (sp->name && !strcmp(sp->name, s))
			return sp;
		//若不在sp在符号表中，sp中变量名赋值为s
		if (!sp->name) {
			sp->name = strdup(s);
			sp->value =0;//未指定变量，为零
			return sp;
		}
	}
	yyerror("TOO MANY SYMBOLS!");
	exit(1);
}

int yylex()
{
	int t ;
	while (1)
	{
		t = getchar ();
		if(t == ' '||t == '\t'||t == '\n')
		{}
		else if(isdigit(t))
		{
			yylval.dval = 0;
			while(isdigit(t)) 
			{
				yylval.dval = yylval.dval * 10 + t - '0';
				t = getchar ();
			}
			ungetc(t,stdin);
			return NUMBER;
		}
		else if((t >= 'a' && t <='z') || (t >= 'A' && t <= 'Z') || (t == '_'))
		{
			int ti = 0;
			while((t >= 'a' && t <='z') || (t >= 'A' && t <= 'Z') || (t == '_') || (t >= '0' && t <= '9'))
			{
				idStr[ti] = t;
				t=getchar();
				ti++;
			}
			idStr[ti] = '\0';
			yylval.symp = symlook(idStr);
			ungetc(t, stdin);
			return NAME;
		}
		else
		{
			return t;
		}
	}
}

int main()
{
	yyin = stdin ;
	do 
	{
		yyparse();
	} 
	while (!feof(yyin));
	return 0;
}
void yyerror(const char* s) 
{
	fprintf (stderr, "Parse error : %s\n", s );
	exit (1);
}
