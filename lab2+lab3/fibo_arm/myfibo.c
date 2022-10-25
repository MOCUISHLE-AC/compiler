//qemu-arm -L /usr/arm-linux-gnueabihf/ ./myfibo
#include<stdio.h>
int n;
//直接开辟内存空间
int fibo[100];
void myfibo(int n)
{
	//fibo的初始两项赋值为1，1
	fibo[0] = 1;
	fibo[1] = 1;
	for (int i = 0;i < n;i++)
	{
		if (i == 0 || i == 1)
			printf("%d ", fibo[i]);
		else
		{
			//前两项相加
			fibo[i] = fibo[i - 1] + fibo[i - 2];
			printf("%d ", fibo[i]);
		}
	}
}
int main()
{
	scanf("%d",&n);
	//对输入进行检验
	if(n<=0||n>100)
	{
		printf("内存erro！！！");
		return 0;
	}
	myfibo(n);
	return 0;
}