%{
/*************************************************************************
author:LHA
Date:10.5
**************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#ifndef YYSTYPE
#define YYSTYPE double
#endif
int yylex();
extern int yyparse();
FILE* yyin;
void yyerror(const char* s );
/*************************************************************************
每个运算符及整数分别定义一个单词类别,
L_PARE='('
R_PARE=')'
**************************************************************************/
%}
%token NUMBER
%token ADD
%token SUB
%token MUL
%token DIV
%token L_PARE
%token R_PARE

%left ADD SUB
%left MUL DIV
%right UMINUS

%%

lines	:	lines expr';' { printf("%f\n", $2); }
		|	lines ';'
		|
		;

expr	:	expr ADD expr { $$ = $1 + $3; }
		|	expr SUB expr { $$ = $1 - $3; }
		|	expr MUL expr { $$ = $1 * $3; }
		|	expr DIV expr { $$ = $1 / $3; }
		|	L_PARE expr R_PARE { $$ = $2; }
		|	SUB expr %prec UMINUS { $$ = -$2; }
		|	NUMBER { $$ = $1; }
		;
%%

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
			yylval = 0;
			while(isdigit(t)) 
			{
				//yylval为number值
				yylval = yylval * 10 + t - '0';
				t = getchar ();
			}
			ungetc(t,stdin);
			//token NUMBER
			return NUMBER;
		}
		else if(t == '+')
		{
			return ADD;
		}
        //依次加入*、/、(、)
		else if(t == '-')
		{
			return SUB;
		}
		else if(t == '*')
		{
			return MUL;
		}
		else if(t == '/')
		{
			return DIV;
		}
		else if(t == '(')
		{
			return L_PARE;
		}
		else if(t == ')')
		{
			return R_PARE;
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