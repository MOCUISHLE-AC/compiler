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
    #include <stack>
    #define mystrlen 40 //定义输出词素的长度
    using namespace std;
    extern FILE *yyin; 
    extern FILE *yyout;
    //yylineno为行数
    int offset=0;//列
    int symbol_sum=0;//符号表数量
    class SymbolTable
    {
        public:
            string name;//变量名
            string* scope;//作用域
    }symbol_table[100];
     //在符号表中查找
    int lookup(SymbolTable target){
        for(int i=0;i<symbol_sum;i++){
            if(symbol_table[i].name==target.name&& (target.scope)==(symbol_table[i].scope)){
                return i;
                }
        }
            return -1;
    }
            //插入到符号表中
    void insert(SymbolTable target){
        if(symbol_sum<100){
            symbol_table[symbol_sum].name=target.name;
            symbol_table[symbol_sum].scope=target.scope;
        }
    }
    // 使用stack来追踪作用域，用栈top元素的地址来标识
    stack<string*> scope_stack;

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


ID [[:alpha:]_][[:alpha:][:digit:]_]*
EOL (\r\n|\n|\r)
WHITE [\t ]

BLOCKCOMMENT_BEGIN "/*"
BLOCKCOMMENT_ELEMENT .|EOL
BLOCKCOMMENT_END "*/"
%x BLOCKCOMMENT

%%

"int" {
    /*
    * Questions: 
    *   Q1: Why we need to return INT in further labs?
    *   Q2: What is "INT" actually?
    */
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"INT \t int\t %d \t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return INT;
    #endif
}
"void" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"VOID \t void \t %d \t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return VOID;
    #endif 
}
"if" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"IF \t if \t %d \t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return IF;
    #endif
};
"else" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"ELSE\t else\t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return ELSE;
    #endif
};
"return" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"RETURN\t return\t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return RETURN;
    #endif
}

"while" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"WHILE \t while \t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return WHILE;
    #endif
}

"break" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"BREAK \t break\t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return BREAK;
    #endif
}

"continue" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"CONTINUE \t continue\t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return CONTINUE;
    #endif
}

"=" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"ASSIGN\t = \t %d \t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return ASSIGN;
    #endif
}
"<" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"LESS \t < \t %d\t %d",yylineno,offset);
           DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return LESS;
    #endif
}
"+" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"ADD\t + \t %d\t %d",yylineno,offset);
           DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return ADD;
    #endif
}

"-" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"SUB\t - \t %d\t %d",yylineno,offset);
           DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return ADD;
    #endif
}

"*" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"MUL\t *\t %d\t %d",yylineno,offset);
           DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return ADD;
    #endif
}

"/" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"DIV\t /\t %d\t %d",yylineno,offset);
           DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return ADD;
    #endif
}

"%" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"MOD\t %%\t %d\t %d",yylineno,offset);
         DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return MOD;
    #endif
}

"++" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"INC\t ++\t %d\t %d",yylineno,offset);
           DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return INC;
    #endif
}
"--" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"DEC\t --\t %d\t %d",yylineno,offset);
           DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return DEC;
    #endif
}
"+=" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"ADDEQU\t +=\t %d\t %d",yylineno,offset);
           DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return ADDEQU;
    #endif
}
"-=" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"SUBEQU\t -=\t %d\t %d",yylineno,offset);
          DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return SUBEQU;
    #endif
}
"==" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"EQUAL\t ==\t %d\t %d",yylineno,offset);
           DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return EQUAL;
    #endif
}
"!=" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"UNEQUAL\t !=\t %d\t %d",yylineno,offset);
         DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return UNEQUAL;
    #endif
}
">=" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"GREEQU\t >=\t %d\t %d",yylineno,offset);
         DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return GREEQU;
    #endif
}
"<=" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"LESEQU\t <=\t %d\t %d",yylineno,offset);
         DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return LESEQU;
    #endif
}
"&&" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"AND\t &&\t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return AND;
    #endif
}
"||" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"OR\t ||\t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return OR;
    #endif
}
"!" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"NOT\t !\t %d\t %d",yylineno,offset);
         DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return NOT;
    #endif
}

";" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"SEMICOLON\t ;\t %d\t %d",yylineno,offset);
         DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return SEMICOLON;
    #endif
}
"(" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"LPAREN\t(\t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return LPAREN;
    #endif
}
")" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"RPAREN\t)\t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
    return RPAREN;
    #endif
}
"{" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"LBRACE\t{ \t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
        string* mystack= new string;
        *mystack="localscope";
        scope_stack.push(mystack);
    #else
        return LBRACE;
    #endif
}
"}" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"RBRACE\t } \t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
        scope_stack.pop();
    #else
        return RBRACE;
    #endif
}
"[" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"LBRACKET\t [ \t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return LBRACKET;
    #endif
}
"]" {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"RBRACKET\t ] \t %d\t %d",yylineno,offset);
         DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return RBRACKET;
    #endif
}
"main" {
	#ifdef ONLY_FOR_LEX
        char temp[100]={0};
        sprintf(temp,"MAIN\t main \t %d\t %d",yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return MAIN;
    #endif
}

{ID} {
    #ifdef ONLY_FOR_LEX
        char temp[100]={0};
        //yytext 匹配模式的文本存储在这一变量中
        // string* back = scope_stack.top();
        // char temp_add[30]={0};
        // sprintf(temp_add, "%#lx", long(back));
        // sprintf(temp,"ID:\t%s \t %d\t%d \t %s", yytext,yylineno,offset,temp_add);
        // DEBUG_FOR_LAB4(temp);
        SymbolTable id_symbol;
        id_symbol.name=yytext;
        id_symbol.scope=scope_stack.top();
        int index=lookup(id_symbol);
        if(index!=-1){
            //已经在符号表中
            sprintf(temp,"ID: \t %s \t %d\t %d \t %p", yytext,yylineno,offset,&symbol_table[index]);
        }
        else{
            //不在符号表中
            insert(id_symbol);
            sprintf(temp,"ID: \t %s \t %d\t %d \t %p", yytext,yylineno,offset,&symbol_table[symbol_sum]);
            symbol_sum++;
        }
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return ID;
    #endif
}

{DECIMIAL} {
	#ifdef ONLY_FOR_LEX
        char temp[mystrlen]={0};
        int num=atoi(yytext);//将字符串转化为一个整数
        sprintf(temp,"DECIMIAL:\t%d \t%d \t %d", num,yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
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
        sprintf(temp,"HEXADECIMAL:\t%d\t %d\t %d",num,yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
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
        sprintf(temp,"OCTONARY:\t%d\t %d \t %d",num,yylineno,offset);
        DEBUG_FOR_LAB4(temp);
        offset += strlen(yytext);
    #else
        return OCTONARY;
    #endif
}
{LINECOMMENT} {
    #ifdef ONLY_FOR_LEX
		//行注释
        char temp[100]="此处为行注释";
        DEBUG_FOR_LAB4(temp);
    #else
        return LINECOMMENT;
    #endif
}
{BLOCKCOMMENT_BEGIN} {BEGIN BLOCKCOMMENT;
        char temp[100]="此处为块注释";
        DEBUG_FOR_LAB4(temp);
}
<BLOCKCOMMENT>{BLOCKCOMMENT_ELEMENT} {}
<BLOCKCOMMENT>{EOL} {yylineno++;}
<BLOCKCOMMENT>{BLOCKCOMMENT_END} {BEGIN INITIAL;}

{EOL} {
    offset += strlen(yytext);
    yylineno++; 
    offset = 0; 
    }
{WHITE} {offset++;}
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
    string start = "global";
    scope_stack.push(&start);
    yylex();
    return 0;
}
#endif
