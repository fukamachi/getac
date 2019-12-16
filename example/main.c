#include<stdio.h>
int main()
{
    int a,b,c;
    char s[101];
    // 整数の入力
    scanf("%d", &a);
    // スペース区切りの整数の入力
    scanf("%d %d",&b,&c);
    // 文字列の入力
    scanf("%s",s);
    // 出力
    printf("%d %s\n",a+b+c,s);
    return 0;
}
