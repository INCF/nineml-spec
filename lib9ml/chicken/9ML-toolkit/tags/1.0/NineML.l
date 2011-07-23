;; -*- Hen -*-
;;
;;  The lexical analyzer for mini-ML.
;;
;;  Based on the code and paper by Xavier Leroy (2000): A modular
;;  module system. Journal of Functional Programming, 10, pp 269-303
;;  doi:10.1017/S0956796800003683
;;
;;
;; Copyright 2010 Ivan Raikov and the Okinawa Institute of
;; Science and Technology.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

upper    [A-Z]
lower    [a-z]
hex      [0-9A-Fa-f]
octal    [0-7]
binary   [0-1]
decimal  [0-9]

%%

;; Lexer rules 

("component"|"module")           (tok (MODULE))
("val"|"binding")                (tok (VALUE))


"function"         (tok (FUNCTION))
"fun"              (tok (FUNCTION))
"return"           (tok (RETURN))
"ret"              (tok (RETURN))
"let"              (tok (LET))
"struct"           (tok (STRUCT))
"end"              (tok (END))
"functor"          (tok (FUNCTOR))
"type"             (tok (TYPE))
"sig"              (tok (SIG))
"in"               (tok (IN))
"if"               (tok (IF))
"then"             (tok (THEN))
"else"             (tok (ELSE))


({lower}|{upper})({lower}|{upper}|_|{decimal})*    (tok (IDENT ,(ident-create yytext)))
`({lower}|{upper})({lower}|{upper}|_|{decimal})*   (tok (LABEL ,(string->symbol (substring yytext 1 (string-length yytext)))))

0(x|X)({hex})+                             (tok (NAT ,(string->number (substring yytext 2 (string-length yytext)) 16)))
0(o|O)({octal})+                           (tok (NAT ,(string->number (substring yytext 2 (string-length yytext)) 8)))
0(b|B)({binary})+                          (tok (NAT ,(string->number (substring yytext 2 (string-length yytext)) 2)))
0(d|D)({decimal})+                         (tok (NAT ,(string->number (substring yytext 2 (string-length yytext)) 10)))

-?(({decimal}+(\.{decimal}+)?)|(\.{decimal}+))([eE]([-+])?{decimal}+)?   (tok (REAL ,(string->number yytext)))

"["                       (let loop ((kont (lambda (x) (tok (SEXPR ,x)))) (result '(#\()))
                             (let ((c (yygetc)))
                               (cond ((eq? 'eof c)    (lexer-error "unexpected end of expression"))
                                     ((char=? #\] c)  (kont (cons #\) result)))
                                     ((char=? #\[ c)  (loop (lambda (x) (loop kont (append x result))) '(#\()))
                                     (else            (loop kont (cons c result)))
                                     )))

"(*"                       (let loop ((kont yycontinue))
                             (let ((c (yygetc)))
                               (cond ((eq? 'eof c) (lexer-error "unexpected end of comment"))
                                     ((and (char=? #\* c) (char=? #\) (yygetc))) (kont))
                                     ((and (char=? #\( c) (char=? #\* (yygetc))) (loop (lambda () (loop kont))))
                                     (else (loop kont)))))

"("                                (tok (LPAREN))
")"                                (tok (RPAREN))
"."                                (tok (DOT))
";"                                (tok (SEMICOLON))
"->"                               (tok (ARROW))
"="                                (tok (EQUAL))
","                                (tok (COMMA))
"'"                                (tok (QUOTE))
":"                                (tok (COLON))
"*"                                (tok (STAR))
"+"                                (tok (PLUS))
"-"                                (tok (MINUS))
"/"                                (tok (SLASH))
"=="                               (tok (EQEQ))
"<>"                               (tok (LG))
"<"                                (tok (LESS))
">"                                (tok (GREATER))
"<="                               (tok (LEQ))
">="                               (tok (GEQ))
\10+                               (yycontinue)
\9+                                (yycontinue)
\13+                               (yycontinue)
\32+                               (yycontinue)



<<EOF>>                            '*eoi*
<<ERROR>>                          (lexer-error (conc yyline ": illegal character") (yygetc))
