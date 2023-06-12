%Trabalho parte I

pvars([x,y,z]).							% definicao das variaveis possiveis (x,y,z)
pvar(X):-pvars(V),member(X,V).

power(X):-pvar(X),!.					% verificar se a variavel e uma potencia
power(X^Y):-pvar(X),integer(Y),Y>1,!.

coefficient(K):-number(K).				% teste de coeficiente

% X e um monomio se:
monomial(X):-pvar(X),!.						% se X e uma variavel singular;
monomial(N):-number(N),!.					% se X e um termo independente (um numero);
monomial(P):-power(P),!.				    % se X e uma potencia;
monomial(K*X):-coefficient(K),power(X),!.   % se X e uma potencia qualquer a multiplicar por um coeficiente

% P e um polinomio se:
polinomial(P):-monomial(P),!.						% se P e um monomio;
polinomial(P+M):-monomial(M),polinomial(P),!.		% se P e uma soma de monomios

% simplificacao de um monomio
simmon(1*P,P):-power(P),!.		% monomio multiplicado por 1 e igual a ele proprio
simmon(-1*P,-P):-power(P),!.    % monomio multiplicado por -1 e igual ao seu simetrico
simmon(0*_,0):-!.               % monomio multiplicado por 0 e igual a 0
simmon(M,M).                    % caso falhe todos os casos, a simplificacao de M e ele proprio

% divisao de um polinomio em termo independente e parte literal
monparts(X^N,1,X^N):-power(X^N),!.
monparts(-X^N,-1,X^N):-power(X^N),!.
monparts(K*P,K,P):-number(K),!.
monparts(K,K,indep):-number(K),!.
monparts(X,1,X):-pvar(X),!.
monparts(-X,-1,X):-pvar(X),!.
% ou seja, do polinomio P= a*b^c resulta (P, a, b^c)

aux_delm(M,X):-monparts(M,_,X),!.                % Predicado auxiliar ao delmonomial. aux_delm(P,X) e verdade sse X e um termo de P
aux_delm(-M,X):-monparts(M,_,X),!.
aux_delm(P+M,X):-(aux_delm(M,X);aux_delm(P,X)),!.
aux_delm(P-M,X):-(aux_delm(M,X);aux_delm(P,X)),!.

% LER PARTE RELATIVA AO PREDICADO delmonomial/4 NO FICHEIRO README.txt
% delmonomial/4 serve para eliminar o monomio de parte literal X de um polinomio dado
% ou seja, do polinomio P= a*b^c + P2 eliminando o monomio de parte literal b^c obtemos P2
% assim, delmonomial(P, b^c, a*b^c, P2)
delmonomial(M,X,M,0):-           % caso base, um monomio M exceto ele proprio e 0
  monomial(M),                   % desque que X seja um monomio
  monparts(M,_,X),!.             % e X seja a parte literal de M
delmonomial(P+M,X,M,P):-not(aux_delm(P,X)),
  monomial(M),
  monparts(M,_,X),!.
delmonomial(P-M,X,M1,P):-not(aux_delm(P,X)),
  monomial(M),
  monparts(M,K,X),K1 is -K,monparts(M1,K1,X),!.
delmonomial(P+M,X,M1,P2+M):-not(aux_delm(M,X)),
  aux_delm(P,X),
  delmonomial(P,X,M1,P2),!.
delmonomial(P-M,X,M1,P2-M):-not(aux_delm(M,X)),
  aux_delm(P,X),
  delmonomial(P,X,(M1),P2),!.
delmonomial(P+M,X,-M1,P2+M):-not(aux_delm(M,X)),
  aux_delm(P,X),
  delmonomial(P,X,-(M1),P2),!.
delmonomial(P-M,X,-M1,P2-M):-not(aux_delm(M,X)),
  aux_delm(P,X),
  delmonomial(P,X,-(M1),P2),!.

% soma de monomios com a mesma parte literal
addmonomial(K1,K2,K3):-       % caso base em que os monomios(k1 e k2) sao constantes
  number(K1),                 % verificar que k1
  number(K2),!,               % e k2 sao numeros
  K3 is (K1)+(K2).            % e assim k3 = k1 + k2

addmonomial(M1,M2,M3):-      % caso onde os monomios (M1 e M2) nao sao constantes
  monparts(M1,K1,XExp),      % averiguar se a parte literal (XExp) de M1
  monparts(M2,K2,XExp),      % e igual a parte literal de M2
  K3 is (K1)+(K2),           % apos essa verificarcao, somamos os coeficientes de cada monomio (k3=(k1)+(k2))
  monparts(M3,K3,XExp).      % obtendo M3 = M1+M2 onde M3 = K3*XExp
% sejam P1=a*b^c  e  P2=d*b^c, entao
% addmonomial(P1, P2, (a+d)*b^c )

% simplificacao de um polinomio:
simpoly_aux(-V-V,Y):-pvar(V),monparts(Y,-2,V),!.
simpoly_aux(P+M,P-M1):-monomial(M),             % correcao dos erros tipo x+-y
  monparts(M,K,XExp),
  K<0,!,
  K1 is -K,
  monparts(M1,K1,XExp).
simpoly_aux(M,M2):-monomial(M),simmon(M,M2),!.  % simplificacao do monomio M para M2, sendo M2=M se M ja estiver simplificado
simpoly_aux(-M,-M2):-monomial(M),simmon(M,M2),!.
simpoly_aux(A*indep,A):-!.
simpoly_aux(-A*indep,-A):-!.
simpoly_aux(A+B*indep,A+B):-!.
simpoly_aux(A-B*indep,A-B):-!.
simpoly_aux(0+P,P1):-simpoly_aux(P,P1),!.       % simplificacao da soma e subtracao de
simpoly_aux(0-P,P1):-simpoly_aux(P,P1),!.       % um polinomio por 0, a sua esquerda e a sua direita,
simpoly_aux(P+0,P1):-simpoly_aux(P,P1),!.       % obtendo o proprio polinomio nesses casos
simpoly_aux(P-0,P1):-simpoly_aux(P,P1),!.       % apos a operacao simplificamos P em P1

simpoly_aux(P+M,P1):-              % simplificar o polinomio P+M
  simpoly_aux(P,P2),               % comecamos por simplificar o polinomio P em P2
  monparts(M,_,XExp),              % considerar XExp como parte literal do monomio M
  delmonomial(P2,XExp,M1,P3),      % eliminar de P2 o monomio com parte literal XExp (M1) obtendo P3
  addmonomial(M,M1,M2),            % somar M com M1 obtendo M2
  delmonomial(P1,XExp,M2,P3),!.    % verificar que P3=P1-M2 e assim P1 e simplificacao do polinomio dado

simpoly_aux(P-M,P2):-              % caso analogo para o polinomio P-M
  simpoly_aux(P,P3),
  monparts(M,K1,XExp),
  delmonomial(P3,XExp,M1,P1),
  K is -K1,
  monparts(M3,K,XExp),
  addmonomial(M3,M1,M2),
  delmonomial(P2,XExp,M2,P1),!.

% casos base de simplificacao:
simpoly_aux(-V+K,Y):-pvar(V),simpoly_aux(K-V,Y),!.
simpoly_aux(-V-K,Y):-pvar(V),monparts(K,KK,Xexp),A is -KK,
  monparts(K1,A,Xexp),
  simpoly_aux(K1-V,Y),!.
simpoly_aux(P+M,P2+M2):-simpoly_aux(P,P2),simmon(M,M2),!.
simpoly_aux(P-M,P2-M2):-simpoly_aux(P,P2),simmon(M,M2),!.
simpoly_aux(-P+M,-P2+M2):-simpoly_aux(P,P2),simmon(M,M2),!.
simpoly_aux(-P-M,-P2-M2):-simpoly_aux(P,P2),simmon(M,M2),!.


simpoly(P,P):-          % caso o polinomio P ja esteja totalmente simplificado
  simpoly_aux(P,P2),    % entao simpoly(P,P) retorna o proprio polinomio
  P==P2,!.
simpoly(P,P3):-         % caso o polinomio P nao esteja totalmente simplificado
  simpoly_aux(P,P2),    % simplificamos o polinomio P em P2, se P2 != P3
  simpoly(P2,P3),!.     % simpoly/2 vai atuando recursivamente ate P estar totalmente simplificado (P=P3)

list2poly([M],M):-monomial(M),!.            % o polinomio associado a uma lista com um monomio, e o proprio monomio
list2poly([-M],-M):-monomial(M),!.          % o mesmo acontece se o monomio for negativo
list2poly(L,P+M):-                          % a lista associada ao polinomio P+M (M->monomio),tem de ter o ultimo termo igual ao monomio
  reverse(L,[M|L1]),                        % e penultimo igual ao ultimo termo do polinomio e por ai adiante
  reverse(L1,L2),                           % o predicado reverse/2 tem o valor de verdade se a segunda lista for a primeira invertida e, assim,
  list2poly(L2,P),                          % o ultimo termo de L sera o primeiro de [M|L1], o que facilita o teste de unificabilidade entre
  monomial(M),!.                            % o monomio e o ultimo elemento da lista
list2poly(L,P-M):-                          % o resto dos casos sao analogos e cobrem as restantes possibilidades relativamente a +/-P+/-M
  reverse(L,[-M|L1]),
  reverse(L1,L2),
  list2poly(L2,P),
  monomial(M),!.
list2poly(L,P-M):-
  reverse(L,[-M|L1]),
  reverse(L1,L2),
  list2poly(L2,P),
  monomial(M),!.
list2poly(L,-P-M):-
  reverse(L,[-M|L1]),
  reverse(L1,[-A|L2]),
  list2poly([A|L2],P),
  monomial(M),!.
list2poly(L,-P+M1):-
  reverse(L,[M1|L1]),
  reverse(L1,[-A|L2]),
  list2poly([A|L2],P),
  monomial(M1),!.

poly2list(P,L):-                    % semelhante ao predicado list2poly/2 mas da forma que foi requesitada pelo trabalho
  list2poly(L,P).

simpoly_list(P,L):-                 % predicado que simplifica uma dada lista associada a um polinomio, numa outra lista associada ao
  list2poly(P,P1),                  % mesmo polinomio mas simplificado
  simpoly(P1,P2),                   % uma vez que o comando de simplificar polinomios ja tinha sido concluido previamente a resolucao deste,
  poly2list(P2,L).                  % decidimos passar da lista para polinomio, simplificar o polinomio e passar, novamente, a lista

addpol(P,M,P1+M):-simpoly(P,P1),       % addpool/2 e um comando auxiliar a adicao de polinomios.
  monomial(M),                         % a soma dum monomio com um polinomio cujos termos nao coincidem, e simplesmente a soma do polinomio com o monomio
  monparts(M,K,XExp),(K>0;K=0),        % NOTA: decidimos simplificar o polinomio por ja sabermos que nos iria ser pedido para simplificar a soma
  not(aux_delm(P,XExp)),!.             % se o polinomio P nao tiver o termo XExp, pode-se usar este caso

addpol(P1,M,P-M1):-monomial(M),simpoly(P1,P),                 % as restantes definicoes sao relativamente a trocas de sinais e somas de polinomios com monomios de termos coincidentes
  monparts(M,K,XExp),
  K<0,
  not(delmonomial(P,XExp,_,_)),!,
  K1 is -K,
  monparts(M1,K1,XExp),!.

addpol(P1,M1,P2-M4):-
  monomial(M1),
  simpoly(P1,P),
  monparts(M1,_,XExp),
  delmonomial(P,XExp,M2,P2),
  addmonomial(M1,M2,M3),
  monparts(M3,K,XExp),(K<0;K=0),
  K1 is -K,
  monparts(M4,K1,XExp),!.

addpol(P2,P1-M1,P4):-
  monomial(M1),
  simpoly(P2,P),
  monparts(M1,K,XExp),(K>0;K=0),
  K1 is -K,
  monparts(M2,K1,XExp),
  addpol(P,M2,P3),
  addpol(P3,P1,P4),!.

addpol(P1,M1,P2+M3):-monomial(M1),
  simpoly(P1,P),
  monparts(M1,_,XExp),
  delmonomial(P,XExp,M2,P2),
  addmonomial(M1,M2,M3),
  monparts(M3,K,XExp),(K>0;K=0),!.

addpol(P2,P10+M1,P4):-monomial(M1),simpoly(P2,P),
  simpoly(P10,P1),
  addpol(P,M1,P3),
  addpol(P3,P1,P4),!.

addpoly(P1,P2,P3):-                        % addpoly(P1,P2,P3) vai somar os polinomios P1 com P2 e obter P3 (simplificado)
  simpoly(P1,P11),simpoly(P2,P22),         % simplificamos primeiro P1 e P2, para ser mais certa a soma.
  addpol(P11,P22,P4),
  simpoly(P4,P3),!.

aux_scalepoly(_,0,0).                       % predicado auxiliar ao poduto de um polinomio por um escalar
aux_scalepoly(P1,K,P2):-monomial(P1),!,     % se P1 for um monomio, o processo e simples e basta "desmontar" o monomio e multiplicar o coeficiente por K
  number(K),                                % voltando a juntar, depois.
  monparts(P1,M1,XExp),
  K1 is K*M1,
  monparts(P2,K1,XExp).
aux_scalepoly(-P1,K,P2):-monomial(P1),!,
  number(K),
  monparts(P1,M1,XExp),
  K1 is -K*M1,
  monparts(P2,K1,XExp).
aux_scalepoly(P1+M,K,P2+M2):-                % reduzir o problema de multiplicar um polinomio por um escalar em multiplicar monomios por escalar
  K > 0,
  monomial(M),
  monparts(M,K1,_),
  K1 > 0,
  aux_scalepoly(M,K,M2),
  aux_scalepoly(P1,K,P2).
aux_scalepoly(P1-M,K,P2-M2):-	             % caso em que o monomio e negativo
  K > 0,
  aux_scalepoly(M,K,M2),
  aux_scalepoly(P1,K,P2).
aux_scalepoly(-M,K,M1):-monomial(M),         % definicao do comando com monomio negativo
  K > 0,
  monparts(M,K1,XExp),
  K2 is -(K1*K),
  monparts(M1,K2,XExp),!.
aux_scalepoly(P1+M,K,P2-M1):-				  % quando se multiplica por um escalar negativo, o sinal tem de trocar mas o processo e analogo.
  K < 0,
  K1 is -K,
  aux_scalepoly(M,K1,M1),
  aux_scalepoly(P1,K,P2).
aux_scalepoly(P1-M,K,P2+M1):-
  K < 0,
  K1 is -K,
  aux_scalepoly(M,K1,M1),
  aux_scalepoly(P1,K,P2).

scalepoly(P,K,P1):-                     % o produto de P com K reduz-se ao mesmo do predicado anterior seguido da simplificacao desse produto
  aux_scalepoly(P,K,P2),
  simpoly(P2,P1),!.

%list2poly convem por parenteses nrs negativos dentro das listas


%Trabalho parte 2
%Relacao entre os nome
relation([X],X):-pvar(X),!.
relation(['times'],*):-!.
relation(['plus'],+):-!.
relation(['minus'],-):-!.
relation(['raised'],^):-!.
relation(['to'],''):-!.
relation(['point'],.):-!.
relation([],0):-!.
relation(['zero'],0):-!.
relation(['one'],1):-!.
relation(['two'],2):-!.
relation(['three'],3):-!.
relation(['four'],4):-!.
relation(['five'],5):-!.
relation(['six'],6):-!.
relation(['seven'],7):-!.
relation(['eight'],8):-!.
relation(['nine'],9):-!.
relation(['ten'],10):-!.
relation(['eleven'],11):-!.
relation(['twelve'],12):-!.
relation(['thirteen'],13):-!.
relation(['fourteen'],14):-!.
relation(['fifteen'],15):-!.
relation(['sixteen'],16):-!.
relation(['seventeen'],17):-!.
relation(['eighteen'],18):-!.
relation(['nineteen'],19):-!.
relation(['twenty'],20):-!.
relation(['thirty'],30):-!.
relation(['forty'],40):-!.
relation(['fifty'],50):-!.
relation(['sixty'],60):-!.
relation(['seventy'],70):-!.
relation(['eighty'],80):-!.
relation(['ninety'],90):-!.
relation([K,hundred],Y):-
  relation([K],Z1),
  Y is 100*Z1,
  Y>99,Y<1000,!.
relation([X1,X2],Y):-
  relation([X1],Y1),
  relation([X2],Y2),
  Y is Y1+Y2,
  Y>20,Y<100,!.
relation([K,hundred,and|L],Y):-
  relation(L,Y1),
  relation([K,hundred],Y2),
  Y is Y1+Y2,
  Y>99,Y<1000,!.
relation([X],Y):-atom_number(X,Y),!.

rfromlist(X-[],X-[]).
rfromlist(X-[XX],Y-YY):-member(XX,X),append(Y,[XX],X1),append(X1,YY,X),!.

rel(X,[Y]):-relation(X,Y),!.
rel([X,point,Y],[Z]):-relation([X],X1),relation([Y],Y1),
  atomic_list_concat([X1,(.),Y1],'',P),term_string(Z,P),!.
rel(X,Y):-rfromlist(X-[times],X1-X2),rel(X1,Y1),rel(X2,Y2),append(Y1,[(*)|Y2],Y),!.
rel(X,Y):-rfromlist(X-[plus],X1-X2),rel(X1,Y1),rel(X2,Y2),append(Y1,[(+)|Y2],Y),!.
rel(X,Y):-rfromlist(X-[minus],X1-X2),rel(X1,Y1),rel(X2,Y2),append(Y1,[(-)|Y2],Y),!.
rel(X,Y):-rfromlist(X-[raised],X1-[to|X2]),rel(X1,Y1),rel(X2,Y2),append(Y1,[(^)|Y2],Y),!.
rel(X,[Y,^,2]):-reverse(X,[squared|X1]),reverse(X1,X2),relation(X2,Y),!.
rel(X,[Y,^,3]):-reverse(X,[cubed|X1]),reverse(X1,X2),relation(X2,Y),!.

list2list([X],[Y]):-atom_string(Y,X).								%Transforma uma lista com aspas ("") para uma sem aspas
list2list([X1|X2],[Y1|Y2]):-atom_string(Y1,X1),list2list(X2,Y2),!.

%list2monomial([X],X):-(number(X);pvar(X)),!.						%Transforma uma lista de monomio num monomio
%list2monomial([-X],-X):-(number(X);pvar(X)),!.
%list2monomial([X,(*),Z],L):-monparts(L,X,Z).
%list2monomial([X],Y):-list2monomial(X,Y),!.
%
%l2m(X,Y):-list2monomial(X,Y),!.
%l2m(LP,Y1+Y2):-rfromlist(LP-[+],X1-X2),l2m(X1,Y1),l2m(X2,Y2),!.
%l2m(LP,Y1-Y2):-rfromlist(LP-[-],X1-X2),l2m(X1,Y1),l2m(X2,Y2),!.

%aux([X],[Y]):-relation([X],Y),!.				%Lista de texto para lista com numeros e valores e simbolos
%aux([X1|X2],[Y1|Y2]):-aux([X1],[Y1]),aux(X2,Y2).

%rem_asp([''],[]):-!.							%Remove as aspas do 'to' da lista (a relacao entre "to" e '' existe e queremos tirar o "to" da lista)
%rem_asp([X],[X]):-!.
%rem_asp([''|X],Y):-rem_asp(X,Y),!.
%rem_asp([X|Y],[X|Z]):-rem_asp(Y,Z),!.

%join_power([X,^,Y|L],[X^Y|L]):-(pvar(X);number(X)),number(Y),!.		%Junta o "raised to" dentra da lista num so elemento
%join_power([X|Y],[X|L]):-join_power(Y,L),!.
%join_power(X,X):-!.

%jpow(X,Y):-join_power(X,Y),X=Y,!.							%Utiliza o comando anterior iterativamente
%jpow(X,Y):-join_power(X,Z),jpow(Z,Y),!.
%jpow(X,X).


% split_sum([X,(+),Y],[[X],[Y]]):-!.				%Transforma uma lista com sinais com uma lista de listas de monomios
% split_sum([X,(+)|Y],[[X]|Z]):-split_sum(Y,Z),!.
% split_sum([X,*,Z,(+)|Y],[[X,*,Z]|K]):-split_sum(Y,K),!.
% split_sum(X,[X]):-!.


text2poly(X,Y):-pol(X,Y),!.									                 %Este comando serve para o polyplay (vai permitir fazer a ligacao com os polinomios guardados)
text2poly(X,Y):-split_string(X,' ',' ',["minus"|L1]),
  list2list(L1,L),
  rel(L,P1),atomic_list_concat([(-)|P1],'',P),term_string(Y,P),!.
text2poly(X,Y):-split_string(X,' ',' ',L1),
  list2list(L1,L),
  rel(L,P1),atomic_list_concat(P1,'',P),term_string(Y,P),!.
text2poly(X,Y):-term_string(Y,X),polinomial(Y),!.			       %Este comando serve para usar com o polyplay (quando se escreve um numero sem ser por extenso)

polyplay:-
  aux_play([],[start]).

:-dynamic(pol/2).

aux_play([],[start]):-
  writeln('Hello, here you can use some simple commands with polynomials'),
  writeln(''),
  writeln('If you wish to see instructions on what you can write, please check "README.txt"'),
  writeln(''),
  writeln('Please enter a string asking instructions regarding polynomials.'),
  flush_output,read(Out),aux_play([],[Out]),!.
aux_play([],[bye]):-
  writeln('See ya!!!'),!.
aux_play([],[X]):-
  split_string(X,' ',' ',Y),list2list(Y,[X1|X2]),
  aux_play([X1],X2),!.
aux_play([A],[]):-(A='help';A='?'),
  writeln('You can write the following commands:'),
  writeln('"add ... with ..." ,"multiply ... by ...","show ... as ...","simplify".'),
  writeln('All of the commands can be used with a "as" at the end to be stored in the computers disk.'),
  writeln('Any help on each of the command, please write "help <command>".'),writeln('Please continue with the instructions.'),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([A],[add|_]):-(A='help';A='?'),
  writeln('The "add" command allows you to add polynomials together as one.'),
  writeln('It also has a few conditions. The conjunction must be using a "with". Here are some examples'),
  writeln('1) "add two times x squared with 5 times y raised to 3"'),
  writeln('2*x^2+5*y^3'),writeln('Please continue with the instructions.'),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([A],[multiply|_]):-(A='help';A='?'),
  writeln('The "multiply" command allows you to multiply a polinomial by a escalar.'),
  writeln('It also has a few conditions. The conjunction must be using a "by". Here are some examples'),
  writeln('1) "multiply two times x by 4"'),
  writeln('		8*x'),writeln('Please continue with the instructions.'),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([A],[show|_]):-(A='help';A='?'),
  writeln('The "show" command allows you see a polynomial as its expanded form. It also allows you to store a polynomial as something you like.'),
  writeln('Here are some examples:'),
  writeln('1) "show two times x plus four times y squared"'),
  writeln('		2*x+4*y^2'),
  writeln('2) "show five times x minus six times y raised to 2 as P1"'),
  writeln('		P1 = 5*x-6*y^2'),writeln('Please continue with the instructions.'),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([A],[simplify|_]):-(A='help';A='?'),
  writeln('The "simplify" command allows you to simplify any given polynomial writen anyway you like (extended or symbolic form).'),
  writeln('Here are some examples:'),
  writeln('1) "simplify two times x squared plus 5 times x raised to 2"'),
  writeln('		6*x^2'),writeln('Please continue with the instructions.'),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([show],L):-member((as),L),!,
  rfromlist(L-[as],L1-[P1]),
  atomic_list_concat(L1,' ',T),text2poly(T,P),
  assert(pol(P1,P)),
  write(P1),write(' = '),writeln(P),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([show],[stored,polynomials]):-
  not(pol(_,_)),writeln("There aren't any stored polynomials at the moment"),
  flush_output,
  read(Out),
  aux_play([],[Out]),!.
aux_play([show],[stored,polynomials]):-
  bagof((X=Y),pol(X,Y),L),writeln(L),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([add],L):-member((as),L),!,
  rfromlist(L-[as],X-[P]),
  rfromlist(X-[with],Y1-Y2),
  atomic_list_concat(Y1,' ',X1),
  atomic_list_concat(Y2,' ',X2),
  text2poly(X1,P1),
  text2poly(X2,P2),
  addpoly(P1,P2,X3),
  assert(pol(P,X3)),
  write(P),write(' = '),writeln(X3),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([add],X):-
  rfromlist(X-[with],Y1-Y2),
  atomic_list_concat(Y1,' ',X1),
  atomic_list_concat(Y2,' ',X2),
  text2poly(X1,P1),
  text2poly(X2,P2),
  addpoly(P1,P2,X3),
  writeln(X3),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([simplify],[polynomial|T]):-member((as),T),
  rfromlist(T-[as],X-[P1]),
  atomic_list_concat(X,' ',X2),
  text2poly(X2,P),
  simpoly(P,X3),
  write(P1),write(' = '),writeln(X3),
  assert(pol(P1,X3)),
  flush_output,read(Out),aux_play([],[Out]),!.
aux_play([simplify],[polynomial|X]):-
  atomic_list_concat(X,' ',X2),
  text2poly(X2,P),
  simpoly(P,X3),
  writeln(X3),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([simplify],X):-
  atomic_list_concat(X,' ',X2),
  text2poly(X2,P),
  simpoly(P,X3),
  writeln(X3),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([show],X):-
  atomic_list_concat(X,' ',X2),
  text2poly(X2,P),
  writeln(P),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([multiply],L):-member((as),L),
  rfromlist(L-[as],X-[P]),
  rfromlist(X-[by],Y1-Y2),
  atomic_list_concat(Y1,' ',X1),
  atomic_list_concat(Y2,' ',X2),
  text2poly(X1,P1),
  text2poly(X2,P2),
  (scalepoly(P1,P2,X3);scalepoly(P2,P1,X3)),
  assert(pol(P,X3)),
  write(P),write(' = '),writeln(X3),flush_output,read(Out),aux_play([],[Out]),!.
aux_play([multiply],X):-
  rfromlist(X-[by],Y1-Y2),
  atomic_list_concat(Y1,' ',X1),
  atomic_list_concat(Y2,' ',X2),
  text2poly(X1,P1),
  text2poly(X2,P2),
  (scalepoly(P1,P2,X3);scalepoly(P2,P1,X3)),
  writeln(X3),flush_output,read(Out),aux_play([],[Out]),!.
aux_play(['forget'],[X]):-retract(pol(X,_)),
  write('The polynomial '),write(X),writeln(' has been forgotten.'),
  writeln('Please enter another instruction.'),
  flush_output,read(Out),aux_play([],[Out]),!.
