%option noyywrap
%{
    /*
    * You will need to comment this line in lab5.
    */
    #define ONLY_FOR_LEX
    
    #ifdef ONLY_FOR_LEX
    #else
    #define YYSTYPE void *
    #include "parser.h"
    #endif

    #define YY_NO_UNPUT
    #define YY_NO_INPUT
    #include <string>

    #ifdef ONLY_FOR_LEX
    #include <ostream>
    #include <fstream>
    #define mystrlen 40 //定义输出词素的长度
    using namespace std;
    extern FILE *yyin; 
    extern FILE *yyout;
    void DEBUG_FOR_LAB4(std::string s){
        std::string DEBUG_INFO = "[DEBUG LAB4]: \t" + s + "\n";
        fputs(DEBUG_INFO.c_str(), yyout);
    }
    #endif
%}

HEXADECIMAL (0x[a-f1-9][a-f0-9]*|0X[A-F1-9][A-F0-9]*|0)
OCTONARY (0[1-7][0-7]*|0)
DECIMIAL ([1-9][0-9]*|0)

LINECOMMENT \/\/[^\n]*
MYBLOCKCOMMENT "/*"([^\*]|(\*)*[^\*/])*(\*)*"*/" 

ID [[:alpha:]_][[:alpha:][:digit:]_]*
EOL (\r\n|\n|\r)
WHITE [\t ]


%x BLOCKCOMMENT

%%

"int" {
    /*
    * Questions: 
    *   Q1: Why we need to return INT in further labs?
    *   Q2: What is "INT" actually?
    */
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("INT\tint");
    #else
        return INT;
    #endif
}
"void" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("VOID\tvoid");
    #else
        return VOID;
    #endif 
}
"if" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("IF\tif");
    #else
        return IF;
    #endif
};
"else" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("ELSE\telse");
    #else
        return ELSE;
    #endif
};
"return" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("RETURN\treturn");
    #else
        return RETURN;
    #endif
}

"while" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("WHILE\twhile");
    #else
        return WHILE;
    #endif
}

"break" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("BREAK\tbreak");
    #else
        return BREAK;
    #endif
}

"continue" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("CONTINUE\tcontinue");
    #else
        return CONTINUE;
    #endif
}

"=" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("ASSIGN\t=");
    #else
        return ASSIGN;
    #endif
}
"<" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("LESS\t<");
    #else
        return LESS;
    #endif
}
"+" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("ADD\t+");
    #else
        return ADD;
    #endif
}

"-" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("SUB\t-");
    #else
        return ADD;
    #endif
}

"*" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("MUL\t*");
    #else
        return ADD;
    #endif
}

"/" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("DIV\t/");
    #else
        return ADD;
    #endif
}

"%" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("MOD\t%");
    #else
        return MOD;
    #endif
}

"++" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("INC\t++");
    #else
        return INC;
    #endif
}
"--" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("DEC\t--");
    #else
        return DEC;
    #endif
}
"+=" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("ADDEQU\t+=");
    #else
        return ADDEQU;
    #endif
}
"-=" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("SUBEQU\t-=");
    #else
        return SUBEQU;
    #endif
}
"==" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("EQUAL\t==");
    #else
        return EQUAL;
    #endif
}
"!=" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("UNEQUAL\t!=");
    #else
        return UNEQUAL;
    #endif
}
">=" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("GREEQU\t>=");
    #else
        return GREEQU;
    #endif
}
"<=" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("LESEQU\t<=");
    #else
        return LESEQU;
    #endif
}
"&&" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("AND\t&&");
    #else
        return AND;
    #endif
}
"||" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("OR\t||");
    #else
        return OR;
    #endif
}
"!" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("NOT\t!");
    #else
        return NOT;
    #endif
}

";" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("SEMICOLON\t;");
    #else
        return SEMICOLON;
    #endif
}
"(" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("LPAREN\t(");
    #else
        return LPAREN;
    #endif
}
")" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("RPAREN\t)");
    #else
    return RPAREN;
    #endif
}
"{" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("LBRACE\t{");
    #else
        return LBRACE;
    #endif
}
"}" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("RBRACE\t}");
    #else
        return RBRACE;
    #endif
}
"[" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("LBRACKET\t[");
    #else
        return LBRACKET;
    #endif
}
"]" {
    #ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("RBRACKET\t]");
    #else
        return RBRACKET;
    #endif
}
"main" {
	#ifdef ONLY_FOR_LEX
        DEBUG_FOR_LAB4("MAIN\tmain");
    #else
        return MAIN;
    #endif
}

{ID} {
    #ifdef ONLY_FOR_LEX
        char temp[mystrlen]="ID:\t";
        //yytext 匹配模式的文本存储在这一变量中
        strcat(temp,yytext);
        DEBUG_FOR_LAB4(temp);
    #else
        return ID;
    #endif
}

{DECIMIAL} {
	#ifdef ONLY_FOR_LEX
        char temp[mystrlen]={0};
        int num=atoi(yytext);//将字符串转化为一个整数
        sprintf(temp,"DECIMIAL:\t%d", num);
        DEBUG_FOR_LAB4(temp);
    #else
        return DECIMIAL;
    #endif
}

{HEXADECIMAL} {
	#ifdef ONLY_FOR_LEX
		char temp[mystrlen]={0};
        strcat(temp,yytext);
        int num=0;
        int i=0;
        while(temp[i]!='\0'){
            if(i==0||i==1){
                i++;
                continue;
            }
            if(temp[i]>='0'&&temp[i]<='9'){
                num=num*16+temp[i]-'0';
            }
            else if(temp[i]>='a'&&temp[i]<='f'){
                num=num*16+temp[i]-'a'+10;
            }
            else if(temp[i]>='A'&&temp[i]<='F'){
                num=num*16+temp[i]-'A'+10;
            }
            i++;
        }
        sprintf(temp,"HEXADECIMAL:\t%d",num);
        DEBUG_FOR_LAB4(temp);
    #else
        return HEXADECIMAL;
    #endif
}

{OCTONARY} {
	#ifdef ONLY_FOR_LEX
		char temp[mystrlen]={0};
        strcat(temp,yytext);
        int num=0;
        int i=0;
        while(temp[i]!='\0'){
            if(i==0){
                i++;
                continue;
            }
            if(temp[i]>='0'&&temp[i]<='7'){
                num=num*8+temp[i]-'0';
                i++;
            }
        }
        sprintf(temp,"OCTONARY:\t%d",num);
        DEBUG_FOR_LAB4(temp);
    #else
        return OCTONARY;
    #endif
}
{LINECOMMENT} {
    #ifdef ONLY_FOR_LEX
		//行注释
        char temp[100]="行注释:";
        strcat(temp,yytext);
        DEBUG_FOR_LAB4(temp);
    #else
        return LINECOMMENT;
    #endif
}
{MYBLOCKCOMMENT} {
    #ifdef ONLY_FOR_LEX
		//块注释
        char temp[100]="块注释:\n";
        strcat(temp,yytext);
        DEBUG_FOR_LAB4(temp);
    #else
        return LINECOMMENT;
    #endif
}


{EOL} yylineno++;
{WHITE}
%%

#ifdef ONLY_FOR_LEX
int main(int argc, char **argv){
    if(argc != 5){
        fprintf(stderr, "Argument Not Enough");
        exit(EXIT_FAILURE);
    }

    if(!(yyin = fopen(argv[1], "r"))){
        fprintf(stderr, "No such file or directory: %s", argv[1]);
        exit(EXIT_FAILURE);
    }

    if(!(yyout = fopen(argv[3], "w"))){
        fprintf(stderr, "No such file or directory: %s", argv[3]);
        exit(EXIT_FAILURE);
    }

    yylex();
    return 0;
}
#endif
