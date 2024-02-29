
D                       [0-9]
L                       [a-zA-Z_.]
H                       [a-fA-F0-9]
E                       [Ee][+-]?{D}+
P                       [Pp][+-]?{D}+
FS                      (f|F|l|L)
IS                      ((u|U)|(u|U)?(l|L|ll|LL)|(l|L|ll|LL)(u|U))

%{
#include <stdio.h>

void count(void);
%}

%%
"/*"                    { comment(); }
"//"[^\n]*              { /* consume //-comment */ }

"%token"                { count(); printf("token %s\n", yytext); }
"%start"                { count(); printf("start %s\n", yytext); }
"%right"                { count(); printf("right %s\n", yytext); }
"%left"                 { count(); printf("left %s\n", yytext); }
"%nonassoc"             { count(); printf("nonassoc %s\n", yytext); }
"%%"                    { count(); printf("%%%% %s\n", yytext); }

{L}({L}|{D})*           { count(); printf("ID %s\n", yytext); }
\"(\\.|[^\\"\n])*\"     { count(); printf("STRING_LITERAL %s\n", yytext); }
'(\\.|[^\'\n])*'        { count(); printf("STRING_LITERAL %s\n", yytext); }

";"                     { count(); printf("; %s\n", yytext); }
"|"                     { count(); printf("| %s\n", yytext); }
":"                     { count(); printf(": %s\n", yytext); }

[ \t\v\n\f]             { count(); }
.                       { /* Add code to complain about unmatched characters */ }

%%


void count(void)
{ }

int yywrap(void)
{
        return 1;
}

void comment(void)
{
        char c, prev = 0;
  
        while ((c = input()) != 0)      /* (EOF maps to 0) */
        {
                if (c == '/' && prev == '*')
                        return;
                prev = c;
        }
        error("unterminated comment");
}


main () {
yylex();
}
