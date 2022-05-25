#include <stdio.h>
#include <time.h>

#define MAX_VALUE 4000000
#define REPORT_VALUE 1000000

int main(void)
{
    int i;
    clock_t start, end;
    double ExecTime;
    start=clock();
    for(i=0;i<MAX_VALUE;i++)
    {
        if(i%REPORT_VALUE==0) printf("%s\n","Stage passed...");
    }
    end=clock();
    ExecTime=(double)(end-start)/CLOCKS_PER_SEC;
    printf("%s%.2lf%s\n","Time elapsed: ",ExecTime,"sec");
    getchar();
    return 0;
}
