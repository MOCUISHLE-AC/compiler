%{
/**********************************
Author:LHA
YACC file
Date: 2022/10/08
***********************************/
#include <iostream>
#include <map>
#include <string>
using namespace std;
#ifndef YYSTYPE
#define YYSTYPE Assemble
#endif

int yylex();
extern int yyparse();
FILE* yyin;
void yyerror(const char* s);
char idStr[50];
map<string, string>character_table;
int basic_addr = 0;

struct Assemble{
	string addr = "";
	string code = "";
    int dval = 0;
    string strval = "";
};
%}

/*
%union {
    double dval;
    string strval;
    Assemble Assemval;
}
*/

%token ID
%token NUMBER
%type expr
%type statement
%type statement_list
%token ASSIGN
%token ADD
%token SUB
%token MUL
%token DIV
%token LEFT_PRA
%token RIGHT_PRA


%right ASSIGN
%left ADD SUB
%left MUL DIV
%right UMINUS

%%

statement_list: statement ';'     { $$.code = $1.code; }  //cout<<$$.code;
        |       statement_list statement ';'   { $$.code = $1.code + $2.code; }  //cout<<$$.code;
        ;                                                                             
statement:      ID ASSIGN expr    { character_table[$1.addr] = "okk";   //若消除空行，这里也应进行判断
                                    $$.code = $3.code + "\nMOV EAX, " + $3.addr + "\nMOV " + $1.addr + ", EAX"; 
                                    cout<<$$.code; }
        |       expr              { $$.code = $1.code; 
                                    cout<<$$.code; }  
        ;

expr : expr ADD expr { basic_addr = basic_addr + 1;  //这里也可以不用tempaddr
                       $$.addr = "0x" + to_string(basic_addr);  //可以在这里设置条件判断消除空行的输出
                       if($1.code!="" && $3.code!="")
                            $$.code = $1.code + '\n' + $3.code + "\nMOV EAX, " + $1.addr + "\nMOV EBX, " + $3.addr + "\nADD EAX, EBX\n" + "MOV " + $$.addr + ", EAX"; 
                       else if($1.code=="" && $3.code=="")
                            $$.code = "\nMOV EAX, " + $1.addr + "\nMOV EBX, " + $3.addr + "\nADD EAX, EBX\n" + "MOV " + $$.addr + ", EAX"; 
                       else
                            $$.code = $1.code + $3.code + "\nMOV EAX, " + $1.addr + "\nMOV EBX, " + $3.addr + "\nADD EAX, EBX\n" + "MOV " + $$.addr + ", EAX"; }
	| expr SUB expr { basic_addr = basic_addr + 1;
                      $$.addr = "0x" + to_string(basic_addr); 
                      $$.code = $1.code + '\n' + $3.code + "\nMOV EAX, " + $1.addr + "\nMOV EBX, " + $3.addr + "\nSUB EAX, EBX\n" + "MOV " + $$.addr + ", EAX"; }
	| expr MUL expr { basic_addr = basic_addr + 1;
                      $$.addr = "0x" + to_string(basic_addr); 
                      $$.code = $1.code + '\n' + $3.code + "\nMOV EAX, " + $1.addr + "\nMOV EBX, " + $3.addr + "\nMUL EAX, EBX\n" + "MOV " + $$.addr + ", EAX"; }
	| expr DIV expr { basic_addr = basic_addr + 1;
                      $$.addr = "0x" + to_string(basic_addr); 
                      $$.code = $1.code + '\n' + $3.code + "\nMOV EAX, " + $1.addr + "\nMOV EBX, " + $3.addr + "\nDIV EAX, EBX\n" + "MOV " + $$.addr + ", EAX"; }
	| LEFT_PRA expr RIGHT_PRA { $$.addr = $2.addr; 
                                $$.code = $2.code; }
	| SUB expr %prec UMINUS { basic_addr = basic_addr + 1;
                              $$.addr = "0x" + to_string(basic_addr);
                              $$.code = $2.code + "\nMOV EAX, " + $2.addr + "NEG EAX\n" + "MOV " + $$.addr + ", EAX"; }
	| NUMBER { $$.addr = to_string($1.dval) + "D"; } //basic_addr = basic_addr + 1;
                                                     //$$.addr = "0x" + to_string(basic_addr);
                                                     //$$.code = "MOV EAX, " + to_string($1.dval) + '\n' + "MOV " + $$.addr + ", EAX";
    | ID  { $$.addr = $1.addr;
            if(character_table[$1.addr] == "")
                $$.code = "MOV EAX, " + to_string(0) + '\n' + "MOV " + $1.addr + ", EAX"; 
            else
                ; }
	;

%%

// programs section


int yylex()
{
    // place your token retrieving code here
    int t;
    while(1)
    {
        t = getchar();
        if(t == ' ' || t == '\t' || t == '\n')
            ;
        else if(isdigit(t)){
            yylval.dval = 0;
            while(isdigit(t)){
                yylval.dval = yylval.dval * 10 + t -'0';
                t = getchar();
            }
            ungetc(t, stdin);
            return NUMBER;
        }
        else if(( t >= 'a' && t <= 'z' ) || ( t >= 'A' && t<= 'Z' ) || ( t == '_' )){
            int i = 0;
            while(( t >= 'a' && t <= 'z' ) || ( t >= 'A' && t<= 'Z' ) || ( t == '_' ) || ( t >= '0' && t <= '9' )){
                idStr[i] = t;
                t = getchar();
                i++;
            }
            idStr[i] = '\0';
            yylval.addr = idStr;
            //cout<<yylval.addr<<endl;
            ungetc(t, stdin);
            //printf("%f\n", character_table[yylval.strval]);
            return ID;
        }
        else{
            switch (t)
            {
            case '+':
                return ADD;
                break;
            case '-':
                return SUB;
                break;
            case '*':
                return MUL;
                break;
            case '/':
                return DIV;
                break;
            case '(':
                return LEFT_PRA;
                break;
            case ')':
                return RIGHT_PRA;
                break;
            case '=':
                return ASSIGN;
                break;
            default:
                return t;
            }
        }
    }
}

int main(void)
{
    yyin = stdin ;
    do {
        yyparse();
        } 
    while (! feof (yyin));
    return 0;
}
void yyerror(const char* s) {
    fprintf (stderr , "Parse error : %s\n", s );
    exit (1);
}
