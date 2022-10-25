struct symtab {
	char *name;
	double value ;
}symtable[100];
//定义符号表为symtable，大小为100

//用于判断是单词，还是符号表中的单词
struct  symtab *symlook();