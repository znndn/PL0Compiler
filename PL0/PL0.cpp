#include "mainwindow.h"
#include <QLineEdit>
#include <QTextEdit>
#include <cstring>
#include <iostream>
#include <QInputDialog>
#include <QRadioButton>

using namespace std;

//---------------------------------------------------------------------------
const  int AL    =  10;  /* LENGTH OF IDENTIFIERS */
const  int NORW  =  18;  /* # OF RESERVED WORDS */  // 关键字个数
const  int TXMAX = 100;  /* LENGTH OF IDENTIFIER TABLE */
const  int NMAX  =  14;  /* MAX NUMBER OF DEGITS IN NUMBERS */
const  int AMAX  =2047;  /* MAXIMUM ADDRESS */
const  int LEVMAX=   3;  /* MAX DEPTH OF BLOCK NESTING */
const  int CXMAX = 200;  /* SIZE OF CODE ARRAY */

const int SYMNUM = 43;  // SYM个数

typedef enum  { NUL, IDENT, NUMBER, PLUS, MINUS, TIMES,
               SLASH, ODDSYM, EQL, NEQ, LSS, LEQ, GTR, GEQ,
               LPAREN, RPAREN, COMMA, SEMICOLON, PERIOD,
               BECOMES, BEGINSYM, ENDSYM, IFSYM, THENSYM,
               WHILESYM, WRITESYM, READSYM, DOSYM, CALLSYM,
               CONSTSYM, VARSYM, PROCSYM, PROGSYM,
               ELSESYM, FORSYM, STEPSYM, UNTILSYM,
               PLUSPLUS, MINUSMINUS, PLUSEQL, MINUSEQL,
               TIMESEQL, SLASHEQL


} SYMBOL;


//typedef  int *SYMSET; // SET OF SYMBOL;
//typedef  char ALFA[11];
//typedef  enum { CONSTANT, VARIABLE, PROCEDUR } OBJECTS ;
//typedef  enum { LIT, OPR, LOD, STO, CAL, INI, JMP, JPC } FCT;
typedef struct {
    FCT F;     /*FUNCTION CODE*/
    int L; 	/*0..LEVMAX  LEVEL*/
    int A;     /*0..AMAX    DISPLACEMENT ADDR*/
} INSTRUCTION;
/* LIT O A -- LOAD CONSTANT A             */
/* OPR 0 A -- EXECUTE OPR A               */
/* LOD L A -- LOAD VARIABLE L,A           */
/* STO L A -- STORE VARIABLE L,A          */
/* CAL L A -- CALL PROCEDURE A AT LEVEL L */
/* INI 0 A -- INCREMET T-REGISTER BY A    */
/* JMP 0 A -- JUMP TO A                   */
/* JPC 0 A -- JUMP CONDITIONAL TO A       */
char   CH;  /*LAST CHAR READ*/
SYMBOL SYM; /*LAST SYMBOL READ*/
ALFA   ID;  /*LAST IDENTIFIER READ*/
int    NUM; /*LAST NUMBER READ*/
int    CC;  /*CHARACTER COUNT*/
int    LL;  /*LINE LENGTH*/
int    CX;  /*CODE ALLOCATION INDEX*/
char   LINE[81];
INSTRUCTION  CODE[CXMAX];
ALFA    KWORD[NORW+1];
SYMBOL  WSYM[NORW+1];
SYMBOL  SSYM['^'+1];
ALFA    MNEMONIC[9];
SYMSET  DECLBEGSYS, STATBEGSYS, FACBEGSYS;

struct {
    ALFA NAME;
    OBJECTS KIND;
    union {
        int VAL;   /*CONSTANT*/
        struct { int LEVEL,ADR,SIZE; } vp;  /*VARIABLE,PROCEDUR:*/
    };
} TABLE[TXMAX];

FILE *FIN,*FOUT;
int ERR;

void EXPRESSION(SYMSET FSYS, int LEV, int &TX);
void TERM(SYMSET FSYS, int LEV, int &TX);
//---------------------------------------------------------------------------
int SymIn(SYMBOL SYM, SYMSET S1) {
    return S1[SYM];
}
//---------------------------------------------------------------------------

SYMSET SymSetUnion(SYMSET S1, SYMSET S2) {
    SYMSET S=(SYMSET)malloc(sizeof(int)*SYMNUM);
    for (int i=0; i<SYMNUM; i++)
        if (S1[i] || S2[i]) S[i]=1;
        else S[i]=0;
    return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetAdd(SYMBOL SY, SYMSET S) {
    SYMSET S1;
    S1=(SYMSET)malloc(sizeof(int)*SYMNUM);
    for (int i=0; i<SYMNUM; i++) S1[i]=S[i];
    S1[SY]=1;
    return S1;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a) {
    SYMSET S; int i,k;
    S=(SYMSET)malloc(sizeof(int)*SYMNUM);
    for (i=0; i<SYMNUM; i++) S[i]=0;
    S[a]=1;
    return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b) {
    SYMSET S; int i,k;
    S=(SYMSET)malloc(sizeof(int)*SYMNUM);
    for (i=0; i<SYMNUM; i++) S[i]=0;
    S[a]=1;  S[b]=1;
    return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c) {
    SYMSET S; int i,k;
    S=(SYMSET)malloc(sizeof(int)*SYMNUM);
    for (i=0; i<SYMNUM; i++) S[i]=0;
    S[a]=1;  S[b]=1; S[c]=1;
    return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c, SYMBOL d) {
    SYMSET S; int i,k;
    S=(SYMSET)malloc(sizeof(int)*SYMNUM);
    for (i=0; i<SYMNUM; i++) S[i]=0;
    S[a]=1;  S[b]=1; S[c]=1; S[d]=1;
    return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c, SYMBOL d,SYMBOL e) {
    SYMSET S; int i,k;
    S=(SYMSET)malloc(sizeof(int)*SYMNUM);
    for (i=0; i<SYMNUM; i++) S[i]=0;
    S[a]=1;  S[b]=1; S[c]=1; S[d]=1; S[e]=1;
    return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c, SYMBOL d,SYMBOL e, SYMBOL f) {
    SYMSET S; int i,k;
    S=(SYMSET)malloc(sizeof(int)*SYMNUM);
    for (i=0; i<SYMNUM; i++) S[i]=0;
    S[a]=1;  S[b]=1; S[c]=1; S[d]=1; S[e]=1; S[f]=1;
    return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNULL() {
    SYMSET S; int i,n,k;
    S=(SYMSET)malloc(sizeof(int)*SYMNUM);
    for (i=0; i<SYMNUM; i++) S[i]=0;
    return S;
}
//---------------------------------------------------------------------------
void MainWindow::Error(int n) {
    QString qs = QString("***");
    for(int i=0;i<CC-1;i++){
        qs.append(" ");
    }
    qs.append("^");
    string s = qs.toStdString();
    // BCB API:static AnsiString StringOfChar(char ch, int count)  返回一个包含count个字符ch的字符串。
    //string s = "***"+AnsiString::StringOfChar(' ', CC-1)+"^";
    //  Form1->printls(s.c_str(),n);   fprintf(FOUT,"%s%d\n", s.c_str(), n);
    printls(s.c_str(),n);   fprintf(FOUT,"%s%d\n", s.c_str(), n);
    ERR++;
} /*Error*/
//---------------------------------------------------------------------------
void MainWindow::GetCh() {
    if (CC==LL) {
        if (feof(FIN)) {
            //Form1->printfs("PROGRAM INCOMPLETE");
            printfs("PROGRAM INCOMPLETE");
            fprintf(FOUT,"PROGRAM INCOMPLETE\n");
            fclose(FOUT);
            exit(0);
        }
        LL=0; CC=0;
        CH=' ';
        while (!feof(FIN) && CH!=10)
        { CH=fgetc(FIN);  LINE[LL++]=CH; }
        LINE[LL-1]=' ';  LINE[LL]=0;
        string s=to_string(CX);
        while(s.length()<3) s=" "+s;
        s=s+" "+LINE;
        //    Form1->printfs(s.c_str());
        printfs(s.c_str());
        fprintf(FOUT,"%s\n",s.c_str());
    }
    CH=LINE[CC++];
} /*GetCh()*/
//---------------------------------------------------------------------------
void MainWindow::GetSym() {
    int i,J,K;   ALFA  A;
    while (CH<=' ') GetCh();
    if (CH>='A' && CH<='Z') { /*ID OR RESERVED WORD*/
        K=0;
        do {
            if (K<AL) A[K++]=CH;
            GetCh();
        }while((CH>='A' && CH<='Z')||(CH>='0' && CH<='9'));
        A[K]='\0';
        strcpy(ID,A); i=1; J=NORW;
        do {
            K=(i+J) / 2;
            if (strcmp(ID,KWORD[K])<=0) J=K-1;
            if (strcmp(ID,KWORD[K])>=0) i=K+1;
        }while(i<=J);
        if (i-1 > J) SYM=WSYM[K];
        else SYM=IDENT;
    }
    else
        if (CH>='0' && CH<='9') { /*NUMBER*/
            K=0; NUM=0; SYM=NUMBER;
            do {
                NUM=10*NUM+(CH-'0');
                K++; GetCh();
            }while(CH>='0' && CH<='9');
            if (K>NMAX) Error(30);
        }
    else
        if (CH==':') {
            GetCh();
            if (CH=='=') { SYM=BECOMES; GetCh(); }
            else SYM=NUL;
        }
        else /* 课内实验：识别多字符运算符 */
            if (CH=='+') {
                GetCh();
                if (CH=='+') { SYM=PLUSPLUS; GetCh(); }
                else if (CH=='=') { SYM=PLUSEQL; GetCh(); }
                else SYM=PLUS;
            }
            else
                if (CH=='-') {
                    GetCh();
                    if (CH=='-') { SYM=MINUSMINUS; GetCh(); }
                    else if (CH=='=') { SYM=MINUSEQL; GetCh(); }
                    else SYM=MINUS;
                }
                else
                    if (CH=='*') {
                        GetCh();
                        if (CH=='=') { SYM=TIMESEQL; GetCh(); }
                        else SYM=TIMES;
                    }
                    else
                        if (CH=='/') {
                            GetCh();
                            if (CH=='=') { SYM=SLASHEQL; GetCh(); }
                            else SYM=SLASH;
                        }
                        else
                            if (CH=='<') {
                                GetCh();
                                if (CH=='=') { SYM=LEQ; GetCh(); }
                                else if (CH=='>') { SYM=NEQ; GetCh(); }
                                else SYM=LSS;
                            }
                            else
                                if (CH=='>') {
                                    GetCh();
                                    if (CH=='=') { SYM=GEQ; GetCh(); }
                                    else SYM=GTR;
                                }
                                else
                                    if (CH=='!') {
                                        GetCh();
                                        if (CH=='=') { SYM=NEQ; GetCh(); }
                                        else SYM=NUL;
                                    }
                                    else { SYM=SSYM[CH]; GetCh(); }
} /*GetSym()*/
//---------------------------------------------------------------------------
void MainWindow::GEN(FCT X, int Y, int Z) {
    if (CX>CXMAX) {
        //    Form1->printfs("PROGRAM TOO LONG");
        printfs("PROGRAM TOO LONG");
        fprintf(FOUT,"PROGRAM TOO LONG\n");
        fclose(FOUT);
        exit(0);
    }
    CODE[CX].F=X; CODE[CX].L=Y; CODE[CX].A=Z;
    CX++;
} /*GEN*/
//---------------------------------------------------------------------------
void MainWindow::TEST(SYMSET S1, SYMSET S2, int N) {
    if (!SymIn(SYM,S1)) {
        Error(N);
        while (!SymIn(SYM,SymSetUnion(S1,S2))) GetSym();
    }
} /*TEST*/
//---------------------------------------------------------------------------
void MainWindow::ENTER(OBJECTS K, int LEV, int &TX, int &DX) { /*ENTER OBJECT INTO TABLE*/
    TX++;
    strcpy(TABLE[TX].NAME,ID); TABLE[TX].KIND=K;
    switch (K) {
    case CONSTANT:
        if (NUM>AMAX) { Error(31); NUM=0; }
        TABLE[TX].VAL=NUM;
        break;
    case VARIABLE:
        TABLE[TX].vp.LEVEL=LEV; TABLE[TX].vp.ADR=DX; DX++;
        break;
    case PROCEDUR:
        TABLE[TX].vp.LEVEL=LEV;
        break;
    }
} /*ENTER*/
//---------------------------------------------------------------------------
int MainWindow::POSITION(ALFA ID, int TX) { /*FIND IDENTIFIER IN TABLE*/
    int i=TX;
    strcpy(TABLE[0].NAME,ID);
    while (strcmp(TABLE[i].NAME,ID)!=0) i--;
    return i;
} /*POSITION*/
//---------------------------------------------------------------------------
void MainWindow::ConstDeclaration(int LEV,int &TX,int &DX) {
    if (SYM==IDENT) {
        GetSym();
        if (SYM==EQL||SYM==BECOMES) {
            if (SYM==BECOMES) Error(1);
            GetSym();
            if (SYM==NUMBER) { ENTER(CONSTANT,LEV,TX,DX); GetSym(); }
            else Error(2);
        }
        else Error(3);
    }
    else Error(4);
} /*ConstDeclaration()*/
//---------------------------------------------------------------------------
void MainWindow::VarDeclaration(int LEV,int &TX,int &DX) {
    if (SYM==IDENT) { ENTER(VARIABLE,LEV,TX,DX); GetSym(); }
    else Error(4);
} /*VarDeclaration()*/
//---------------------------------------------------------------------------
void MainWindow::ListCode(int CX0) {  /*LIST CODE GENERATED FOR THIS Block*/
    if(visibleRadioButton->isChecked())
        for (int i=CX0; i<CX; i++) {
            string s=to_string(i);
            while(s.length()<3)s=" "+s;
            s=s+" "+MNEMONIC[CODE[i].F]+" "+to_string(CODE[i].L)+" "+to_string(CODE[i].A);
            //      Form1->printfs(s.c_str());
            printfs(s.c_str());
            //      fprintf(FOUT,"%3d%5s%4d%4d\n",i,MNEMONIC[CODE[i].F],CODE[i].L,CODE[i].A);
            fprintf(FOUT,"%s\n",s.c_str());
        }
} /*ListCode()*/
//---------------------------------------------------------------------------
void MainWindow::FACTOR(SYMSET FSYS, int LEV, int &TX) {
    int i;
    TEST(FACBEGSYS,FSYS,24);
    while (SymIn(SYM,FACBEGSYS)) {

        // 1. 处理前缀 ++ 和 -- (例如: ++a, --a)
        if (SYM == PLUSPLUS || SYM == MINUSMINUS) {
            SYMBOL op = SYM;
            GetSym();
            if (SYM == IDENT) {
                i = POSITION(ID, TX);
                if (i == 0) Error(11); // 变量未声明
                else if (TABLE[i].KIND != VARIABLE) Error(12); // 非变量不能自增减
                else {
                    GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); // 取出原值
                    GEN(LIT, 0, 1);                                     // 准备数字 1
                    if (op == PLUSPLUS) GEN(OPR, 0, 2);                 // 加 1
                    else GEN(OPR, 0, 3);                                // 减 1
                    GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); // 存回变量更新内存

                    GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); // 再次取出新值，留在栈顶作为表达式结果
                }
                GetSym();
            } else {
                Error(14); // ++ 后面期待跟一个标识符
            }
        }
        // 2. 处理普通的标识符，以及后缀 ++ 和 -- (例如: a++, a--)
        else if (SYM == IDENT) {
            i = POSITION(ID, TX);
            if (i == 0) Error(11);
            else
                switch (TABLE[i].KIND) {
                case CONSTANT:
                    GEN(LIT, 0, TABLE[i].VAL);
                    break;
                case VARIABLE:
                    GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); // 取出值（如果是变量，此时原值已在栈顶）
                    break;
                case PROCEDUR:
                    Error(21);
                    break;
                }
            GetSym();

            // 判断标识符之后是否紧接着后缀的 ++ 或 --
            if (i != 0 && TABLE[i].KIND == VARIABLE && (SYM == PLUSPLUS || SYM == MINUSMINUS)) {
                // 注意：此时栈顶已经有了一个刚刚 LOD 进来的原值。
                // 此时不对它做任何操作，把它压在栈底下作为最终表达式的值！
                // 接着进行一次自增/自减的内存修改操作：
                GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); // 第二次取出值
                GEN(LIT, 0, 1);                                     // 准备数字 1
                if (SYM == PLUSPLUS) GEN(OPR, 0, 2);                // 相加
                else GEN(OPR, 0, 3);                                // 相减
                GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); // 存回内存，此时计算所用的数据退栈

                GetSym(); // 读取下一个符号
            }
        }
        // 3. 处理数字
        else if (SYM == NUMBER) {
            if (NUM > AMAX) { Error(31); NUM = 0; }
            GEN(LIT, 0, NUM);
            GetSym();
        }
        // 4. 处理括号表达式
        else if (SYM == LPAREN) {
            GetSym();
            EXPRESSION(SymSetAdd(RPAREN, FSYS), LEV, TX);
            if (SYM == RPAREN) GetSym();
            else Error(22);
        }

        TEST(FSYS, FACBEGSYS, 23);
    }
}
//---------------------------------------------------------------------------
void MainWindow::TERM(SYMSET FSYS, int LEV, int &TX) {  /*TERM*/
    SYMBOL MULOP;
    FACTOR(SymSetUnion(FSYS,SymSetNew(TIMES,SLASH)), LEV,TX);
    while (SYM==TIMES || SYM==SLASH) {
        MULOP=SYM;  GetSym();
        FACTOR(SymSetUnion(FSYS,SymSetNew(TIMES,SLASH)),LEV,TX);
        if (MULOP==TIMES) GEN(OPR,0,4);
        else GEN(OPR,0,5);
    }
} /*TERM*/
//---------------------------------------------------------------------------
void MainWindow::EXPRESSION(SYMSET FSYS, int LEV, int &TX) {
    SYMBOL ADDOP;
    if (SYM==PLUS || SYM==MINUS) {
        ADDOP=SYM; GetSym();
        TERM(SymSetUnion(FSYS,SymSetNew(PLUS,MINUS)),LEV,TX);
        if (ADDOP==MINUS) GEN(OPR,0,1);
    }
    else TERM(SymSetUnion(FSYS,SymSetNew(PLUS,MINUS)),LEV,TX);
    while (SYM==PLUS || SYM==MINUS) {
        ADDOP=SYM; GetSym();
        TERM(SymSetUnion(FSYS,SymSetNew(PLUS,MINUS)),LEV,TX);
        if (ADDOP==PLUS) GEN(OPR,0,2);
        else GEN(OPR,0,3);
    }
} /*EXPRESSION*/
//---------------------------------------------------------------------------
void MainWindow::CONDITION(SYMSET FSYS,int LEV,int &TX) {
    SYMBOL RELOP;
    if (SYM==ODDSYM) { GetSym(); EXPRESSION(FSYS,LEV,TX); GEN(OPR,0,6); }
    else {
        EXPRESSION(SymSetUnion(SymSetNew(EQL,NEQ,LSS,LEQ,GTR,GEQ),FSYS),LEV,TX);
        if (!SymIn(SYM,SymSetNew(EQL,NEQ,LSS,LEQ,GTR,GEQ))) Error(20);
        else {
            RELOP=SYM; GetSym(); EXPRESSION(FSYS,LEV,TX);
            switch (RELOP) {
            case EQL: GEN(OPR,0,8);  break;
            case NEQ: GEN(OPR,0,9);  break;
            case LSS: GEN(OPR,0,10); break;
            case GEQ: GEN(OPR,0,11); break;
            case GTR: GEN(OPR,0,12); break;
            case LEQ: GEN(OPR,0,13); break;
            }
        }
    }
} /*CONDITION*/
//---------------------------------------------------------------------------
void MainWindow::STATEMENT(SYMSET FSYS,int LEV,int &TX) {   /*STATEMENT*/
    int i,CX1,CX2,CX3;
    switch (SYM) {
    case IDENT:
        i=POSITION(ID,TX);
        if (i==0) Error(11);
        else
            if (TABLE[i].KIND!=VARIABLE) { /*ASSIGNMENT TO NON-VARIABLE*/
                Error(12); i=0;
            }
        GetSym();

        // 判断是否是合法的赋值运算符 (:=, *=, /=)
        if (SYM == BECOMES || SYM == TIMESEQL || SYM == SLASHEQL) {

            SYMBOL op = SYM; // 记录下具体是哪种赋值运算符
            GetSym();        // 读取下一个单词

            // 如果是复合赋值(+=, -=, *=, /=)，需要先将变量原本的值压入栈顶 (LOD)
            if (op != BECOMES && i != 0) {
                GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
            }

            // 解析等号右侧的表达式 (表达式的值会自动计算并压入栈顶)
            EXPRESSION(FSYS, LEV, TX);

            // 目标代码生成
            if (i != 0) {
                // 如果是复合赋值，根据运算符生成对应的计算指令
                if (op == TIMESEQL) GEN(OPR, 0, 4); // 栈顶两元素相乘 (a * b)
                else if (op == SLASHEQL) GEN(OPR, 0, 5); // 栈顶两元素相除 (a / b)

                // 最后，无论哪种赋值，都将栈顶结果存回变量所在地址 (STO)
                GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
            }
        }
        else {
            Error(13); // 如果不是赋值运算符，抛出缺失赋值号的错误
        }
        break;
    case READSYM:
        GetSym();
        if (SYM!=LPAREN) Error(34);
        else
            do {
                GetSym();
                if (SYM==IDENT) i=POSITION(ID,TX);
                else i=0;
                if (i==0) Error(35);
                else {
                    GEN(OPR,0,16);
                    GEN(STO,LEV-TABLE[i].vp.LEVEL,TABLE[i].vp.ADR);
                }
                GetSym();
            }while(SYM==COMMA);
        if (SYM!=RPAREN) {
            Error(33);
            while (!SymIn(SYM,FSYS)) GetSym();
        }
        else GetSym();
        break; /* READSYM */
    case WRITESYM:
        GetSym();
        if (SYM==LPAREN) {
            do {
                GetSym();
                EXPRESSION(SymSetUnion(SymSetNew(RPAREN,COMMA),FSYS),LEV,TX);
                GEN(OPR,0,14);
            }while(SYM==COMMA);
            if (SYM!=RPAREN) Error(33);
            else GetSym();
        }
        GEN(OPR,0,15);
        break; /*WRITESYM*/
    case CALLSYM:
        GetSym();
        if (SYM!=IDENT) Error(14);
        else {
            i=POSITION(ID,TX);
            if (i==0) Error(11);
            else
                if (TABLE[i].KIND==PROCEDUR)
                    GEN(CAL,LEV-TABLE[i].vp.LEVEL,TABLE[i].vp.ADR);
                else Error(15);
            GetSym();
        }
        break;
    case IFSYM:
        GetSym();
        CONDITION(SymSetUnion(SymSetNew(THENSYM,DOSYM),FSYS),LEV,TX);
        if (SYM==THENSYM) GetSym();
        else Error(16);

        // 生成条件假时的跳转指令（此时还不知道跳去哪，地址暂时填0，记录指令位置为CX1）
        CX1=CX;  GEN(JPC,0,0);

        // 解析 THEN 后面的语句。注意这里要把 ELSESYM 加入跟随符号集，防止报错
        STATEMENT(SymSetUnion(SymSetNew(ELSESYM), FSYS), LEV, TX);

        // 判断是否跟着 ELSE
        if (SYM == ELSESYM) {
            GetSym();
            // THEN 语句执行完后，需要无条件跳转到 ELSE 之后。暂填0，记录位置为 CX2
            CX2=CX;  GEN(JMP,0,0);

            // 此时的 CX (当前代码指针) 就是 ELSE 语句的开始地址，把这个地址回填给前面的 JPC
            CODE[CX1].A=CX;

            // 解析 ELSE 后面的语句
            STATEMENT(FSYS, LEV, TX);

            // ELSE 语句解析完了，此时的 CX 是整个 IF-ELSE 结束后的第一条指令，把它回填给 JMP
            CODE[CX2].A=CX;
        } else {
            // 如果没有 ELSE，那么条件为假时就直接跳转到 THEN 语句之后的位置
            CODE[CX1].A=CX;
        }
        break;
    case BEGINSYM:
        GetSym();
        STATEMENT(SymSetUnion(SymSetNew(SEMICOLON,ENDSYM),FSYS),LEV,TX);
        while (SymIn(SYM, SymSetAdd(SEMICOLON,STATBEGSYS))) {
            if (SYM==SEMICOLON) GetSym();
            else Error(10);
            STATEMENT(SymSetUnion(SymSetNew(SEMICOLON,ENDSYM),FSYS),LEV,TX);
        }
        if (SYM==ENDSYM) GetSym();
        else Error(17);
        break;
    case WHILESYM:
        CX1=CX; GetSym(); CONDITION(SymSetAdd(DOSYM,FSYS),LEV,TX);
        CX2=CX; GEN(JPC,0,0);
        if (SYM==DOSYM) GetSym();
        else Error(18);
        STATEMENT(FSYS,LEV,TX);
        GEN(JMP,0,CX1);
        CODE[CX2].A=CX;
        break;
    case FORSYM:
        GetSym(); // 读入 FOR

        // 1. 解析 <变量>
        if (SYM == IDENT) {
            i = POSITION(ID, TX);
            if (i == 0) Error(11); // 变量未声明
            else if (TABLE[i].KIND != VARIABLE) Error(12); // 非变量不能被赋值
            GetSym();
        } else {
            Error(14); // 期待标识符
            i = 0;
        }

        // 2. 解析 :=
        if (SYM == BECOMES) {
            GetSym();
        } else {
            Error(13); // 期待 :=
        }

        // 3. 解析初值 <表达式>
        // 把 STEPSYM 加入容错集合，防止表达式解析过头
        EXPRESSION(SymSetUnion(SymSetNew(STEPSYM), FSYS), LEV, TX);
        if (i != 0) GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); // i := 初值

        // === 生成跳转指令 ===
        CX1 = CX; // 记录 JMP 的位置，稍后回填 L_Cond
        GEN(JMP, 0, 0); // 第一次进循环跳过步长增加，直接去判断条件

        // 4. 解析 STEP <表达式>
        CX2 = CX; // 记录 L_Step 位置 (每次循环体结束后跳回这里加步长)
        if (SYM == STEPSYM) {
            GetSym();
        } else {
            Error(0); // 缺少 STEP (此处用通用错误替代，或自定义错误码)
        }

        if (i != 0) GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); // 将 i 入栈
        EXPRESSION(SymSetUnion(SymSetNew(UNTILSYM), FSYS), LEV, TX); // 计算步长表达式
        GEN(OPR, 0, 2); // 栈顶两元素相加 (i + step)
        if (i != 0) GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); // 存回 i: i := i + step

        // 5. 解析 UNTIL <表达式>
        CX3 = CX; // 记录 L_Cond 位置
        CODE[CX1].A = CX3; // [回填] 把 L_Cond 的地址填给最初的那个 JMP

        if (SYM == UNTILSYM) {
            GetSym();
        } else {
            Error(0); // 缺少 UNTIL
        }

        if (i != 0) GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); // 将 i 入栈
        EXPRESSION(SymSetUnion(SymSetNew(DOSYM), FSYS), LEV, TX); // 计算终值表达式
        GEN(OPR, 0, 13); // 判断: 栈顶变成 (i <= 终值) 的布尔值

        CX1 = CX; // 记录 JPC 的位置，稍后回填 L_End
        GEN(JPC, 0, 0); // 若 i > 终值，条件为假(0)，跳出循环

        // 6. 解析 DO <语句>
        if (SYM == DOSYM) {
            GetSym();
        } else {
            Error(18); // 缺少 DO
        }

        STATEMENT(FSYS, LEV, TX); // 递归解析 DO 里面的循环体语句

        GEN(JMP, 0, CX2); // 循环体执行完，无条件跳回步长计算入口 L_Step

        CODE[CX1].A = CX; // [回填] 此时的 CX 已经跳出了循环，将其回填给跳出循环的 JPC (L_End)
        break;
    }
    TEST(FSYS,SymSetNULL(),19);
} /*STATEMENT*/
//---------------------------------------------------------------------------
void MainWindow::Block(int LEV, int TX, SYMSET FSYS) {
    int DX=3;    /*DATA ALLOCATION INDEX*/
    int TX0=TX;  /*INITIAL TABLE INDEX*/
    int CX0=CX;  /*INITIAL CODE INDEX*/
    TABLE[TX].vp.ADR=CX; GEN(JMP,0,0);
    if (LEV>LEVMAX) Error(32);
    do {
        if (SYM==CONSTSYM) {
            GetSym();
            do {
                ConstDeclaration(LEV,TX,DX);
                while (SYM==COMMA) {
                    GetSym();  ConstDeclaration(LEV,TX,DX);
                }
                if (SYM==SEMICOLON) GetSym();
                else Error(5);
            }while(SYM==IDENT);
        }
        if (SYM==VARSYM) {
            GetSym();
            do {
                VarDeclaration(LEV,TX,DX);
                while (SYM==COMMA) { GetSym(); VarDeclaration(LEV,TX,DX); }
                if (SYM==SEMICOLON) GetSym();
                else Error(5);
            }while(SYM==IDENT);
        }
        while ( SYM==PROCSYM) {
            GetSym();
            if (SYM==IDENT) { ENTER(PROCEDUR,LEV,TX,DX); GetSym(); }
            else Error(4);
            if (SYM==SEMICOLON) GetSym();
            else Error(5);
            Block(LEV+1,TX,SymSetAdd(SEMICOLON,FSYS));
            if (SYM==SEMICOLON) {
                GetSym();
                TEST(SymSetUnion(SymSetNew(IDENT,PROCSYM),STATBEGSYS),FSYS,6);
            }
            else Error(5);
        }
        TEST(SymSetAdd(IDENT,STATBEGSYS), DECLBEGSYS,7);
    }while(SymIn(SYM,DECLBEGSYS));
    CODE[TABLE[TX0].vp.ADR].A=CX;
    TABLE[TX0].vp.ADR=CX;   /*START ADDR OF CODE*/
    TABLE[TX0].vp.SIZE=DX;  /*SIZE OF DATA SEGMENT*/
    GEN(INI,0,DX);
    STATEMENT(SymSetUnion(SymSetNew(SEMICOLON,ENDSYM),FSYS),LEV,TX);
    GEN(OPR,0,0);  /*RETURN*/
    TEST(FSYS,SymSetNULL(),8);
    ListCode(CX0);
} /*Block*/
//---------------------------------------------------------------------------
int MainWindow::BASE(int L,int B,int S[]) {
    int B1=B; /*FIND BASE L LEVELS DOWN*/
    while (L>0) { B1=S[B1]; L=L-1; }
    return B1;
} /*BASE*/
//---------------------------------------------------------------------------
void MainWindow::Interpret() {
    const int STACKSIZE = 500;
    int P,B,T; 		/*PROGRAM BASE TOPSTACK REGISTERS*/
    INSTRUCTION I;
    int S[STACKSIZE];  	/*DATASTORE*/
    //  Form1->printfs("~~~ RUN PL0 ~~~");
    printfs("~~~ RUN PL0 ~~~");
    fprintf(FOUT,"~~~ RUN PL0 ~~~\n");
    T=0; B=1; P=0;
    S[1]=0; S[2]=0; S[3]=0;
    do {
        I=CODE[P]; P=P+1;
        switch (I.F) {
        case LIT: T++; S[T]=I.A; break;
        case OPR:
            switch (I.A) { /*OPERATOR*/
            case 0: /*RETURN*/ T=B-1; P=S[T+3]; B=S[T+2]; break;
            case 1: S[T]=-S[T];  break;
            case 2: T--; S[T]=S[T]+S[T+1];   break;
            case 3: T--; S[T]=S[T]-S[T+1];   break;
            case 4: T--; S[T]=S[T]*S[T+1];   break;
            case 5: T--; S[T]=S[T]/S[T+1]; break;
            case 6: S[T]=(S[T]%2!=0);        break;
            case 8: T--; S[T]=S[T]==S[T+1];  break;
            case 9: T--; S[T]=S[T]!=S[T+1];  break;
            case 10: T--; S[T]=S[T]<S[T+1];   break;
            case 11: T--; S[T]=S[T]>=S[T+1];  break;
            case 12: T--; S[T]=S[T]>S[T+1];   break;
            case 13: T--; S[T]=S[T]<=S[T+1];  break;
            case 14: printls("",S[T]); fprintf(FOUT,"%d\n",S[T]); T--;
                break;
            case 15: /*Form1->printfs(""); fprintf(FOUT,"\n"); */ break;
            case 16: T++;  //S[T]=InputBox("输入","请键盘输入：", 0).ToInt();
                bool ok;
                int in=QInputDialog::getInt(this,tr(" 输入"),tr("请键盘输入："),0,0,200,2,&ok);
                if(ok){
                    S[T]=in;
                }else{
                    S[T]=0;
                }
                printls("? ",S[T]); fprintf(FOUT,"? %d\n",S[T]);
                break;
            }
            break;
        case LOD: T++; S[T]=S[BASE(I.L,B,S)+I.A]; break;
        case STO: S[BASE(I.L,B,S)+I.A]=S[T]; T--; break;
        case CAL: /*GENERAT NEW Block MARK*/
            S[T+1]=BASE(I.L,B,S); S[T+2]=B; S[T+3]=P;
            B=T+1; P=I.A; break;
        case INI: T=T+I.A;  break;
        case JMP: P=I.A; break;
        case JPC: if (S[T]==0) P=I.A;  T--;  break;
        } /*switch*/
    }while(P!=0);
    //  Form1->printfs("~~~ END PL0 ~~~");
    printfs("~~~ END PL0 ~~~");
    fprintf(FOUT,"~~~ END PL0 ~~~\n");
} /*Interpret*/
//---------------------------------------------------------------------------

void MainWindow::runClicked(){
    for (CH=' '; CH<='^'; CH++) SSYM[CH]=NUL;
    strcpy(KWORD[ 1],"BEGIN");    strcpy(KWORD[ 2],"CALL");
    strcpy(KWORD[ 3],"CONST");    strcpy(KWORD[ 4],"DO");
    strcpy(KWORD[ 5],"ELSE");     strcpy(KWORD[ 6],"END");
    strcpy(KWORD[ 7],"FOR");      strcpy(KWORD[ 8],"IF");
    strcpy(KWORD[ 9],"ODD");      strcpy(KWORD[10],"PROCEDURE");
    strcpy(KWORD[11],"PROGRAM");  strcpy(KWORD[12],"READ");
    strcpy(KWORD[13],"STEP");     strcpy(KWORD[14],"THEN");
    strcpy(KWORD[15],"UNTIL");    strcpy(KWORD[16],"VAR");
    strcpy(KWORD[17],"WHILE");    strcpy(KWORD[18],"WRITE");

    WSYM[ 1]=BEGINSYM;   WSYM[ 2]=CALLSYM;
    WSYM[ 3]=CONSTSYM;   WSYM[ 4]=DOSYM;
    WSYM[ 5]=ELSESYM;    WSYM[ 6]=ENDSYM;
    WSYM[ 7]=FORSYM;     WSYM[ 8]=IFSYM;
    WSYM[ 9]=ODDSYM;     WSYM[10]=PROCSYM;
    WSYM[11]=PROGSYM;    WSYM[12]=READSYM;
    WSYM[13]=STEPSYM;    WSYM[14]=THENSYM;
    WSYM[15]=UNTILSYM;   WSYM[16]=VARSYM;
    WSYM[17]=WHILESYM;   WSYM[18]=WRITESYM;

    SSYM['(']=LPAREN;    SSYM[')']=RPAREN;
    SSYM['=']=EQL;       SSYM[',']=COMMA;
    SSYM['.']=PERIOD;    SSYM[';']=SEMICOLON;

    strcpy(MNEMONIC[LIT],"LIT");   strcpy(MNEMONIC[OPR],"OPR");
    strcpy(MNEMONIC[LOD],"LOD");   strcpy(MNEMONIC[STO],"STO");
    strcpy(MNEMONIC[CAL],"CAL");   strcpy(MNEMONIC[INI],"INI");
    strcpy(MNEMONIC[JMP],"JMP");   strcpy(MNEMONIC[JPC],"JPC");

    DECLBEGSYS=(int*)malloc(sizeof(int)*SYMNUM);
    STATBEGSYS=(int*)malloc(sizeof(int)*SYMNUM);
    FACBEGSYS =(int*)malloc(sizeof(int)*SYMNUM);
    for(int j=0; j<SYMNUM; j++) {
        DECLBEGSYS[j]=0;  STATBEGSYS[j]=0;  FACBEGSYS[j] =0;
    }
    DECLBEGSYS[CONSTSYM]=1;
    DECLBEGSYS[VARSYM]=1;
    DECLBEGSYS[PROCSYM]=1;
    STATBEGSYS[BEGINSYM]=1;
    STATBEGSYS[CALLSYM]=1;
    STATBEGSYS[IFSYM]=1;
    STATBEGSYS[WHILESYM]=1;
    STATBEGSYS[WRITESYM]=1;
    STATBEGSYS[FORSYM]=1;  // 把 FOR 加入语句起始集合，防止语法报错无法恢复
    FACBEGSYS[IDENT] =1;
    FACBEGSYS[NUMBER]=1;
    FACBEGSYS[LPAREN]=1;
    FACBEGSYS[PLUSPLUS]=1;
    FACBEGSYS[MINUSMINUS]=1;

    const string filename = sourceLineEdit->text().toStdString();
    const string pl0name = filename+".PL0";
    const string codname = filename+".COD";
    if ((FIN=fopen(pl0name.c_str(),"r"))!=0) {
        FOUT=fopen(codname.c_str(),"w");
        //Form1->printfs("=== COMPILE PL0 ===");
        printfs("=== COMPILE PL0 ===");
        fprintf(FOUT,"=== COMPILE PL0 ===\n");
        ERR=0;
        CC=0; CX=0; LL=0; CH=' '; GetSym();
        if (SYM!=PROGSYM) Error(0);
        else {
            GetSym();
            if (SYM!=IDENT) Error(0);
            else {
                GetSym();
                if (SYM!=SEMICOLON) Error(5);
                else GetSym();
            }
        }
        Block(0,0,SymSetAdd(PERIOD,SymSetUnion(DECLBEGSYS,STATBEGSYS)));
        if (SYM!=PERIOD) Error(9);
        if (ERR==0) Interpret();
        else {
            //Form1->printfs("ERROR IN PL/0 PROGRAM");
            printfs("ERROR IN PL/0 PROGRAM");
            fprintf(FOUT,"ERROR IN PL/0 PROGRAM");
        }
        fprintf(FOUT,"\n"); fclose(FOUT);
    }
}

