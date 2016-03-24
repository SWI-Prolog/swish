/*
Throwing a coin with uncertainty on its fairness, from
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated 
disjunctions. In International Conference on Logic Programming, 
volume 3131 of LNCS, pages 195-209. Springer, 2004.
*/
:- use_module(library(real)).
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

iris(I,X1,X2,X3,X4):-
  prior_cat([1,1,1],Par),
  latent_class(I,Par,K),
  mean(K,x1,M1),
  mean(K,x2,M2),
  mean(K,x3,M3),
  mean(K,x4,M4),
  measure(x1,I,K,M1,X1),
  measure(x2,I,K,M2,X2),
  measure(x3,I,K,M3,X3),
%
  measure(x4,I,K,M4,X4).

prior_cat(Alfas,Values):dirichlet(Values,Alfas).


mean(_,_,M): gaussian(M,0,1).

measure(_,_,_,M,A): gaussian(A,M,1.0).

latent_class(I,Par,V):-
  maplist(pair_val,[1,2,3],Par,D),
  latc(I,D,V).

latc(_,D,V):discrete(V,D).

pair_val(V,P,V:P).

:- end_lpad.
data(5.1,3.5,1.4,0.2,setosa).
data(4.9,3.0,1.4,0.2,setosa).
data(4.7,3.2,1.3,0.2,setosa).
data(4.6,3.1,1.5,0.2,setosa).
data(5.0,3.6,1.4,0.2,setosa).
data(5.4,3.9,1.7,0.4,setosa).
data(4.6,3.4,1.4,0.3,setosa).
data(5.0,3.4,1.5,0.2,setosa).
data(4.4,2.9,1.4,0.2,setosa).
data(4.9,3.1,1.5,0.1,setosa).
data(5.4,3.7,1.5,0.2,setosa).
data(4.8,3.4,1.6,0.2,setosa).
data(4.8,3.0,1.4,0.1,setosa).
data(4.3,3.0,1.1,0.1,setosa).
data(5.8,4.0,1.2,0.2,setosa).
data(5.7,4.4,1.5,0.4,setosa).
data(5.4,3.9,1.3,0.4,setosa).
data(5.1,3.5,1.4,0.3,setosa).
data(5.7,3.8,1.7,0.3,setosa).
data(5.1,3.8,1.5,0.3,setosa).
data(5.4,3.4,1.7,0.2,setosa).
data(5.1,3.7,1.5,0.4,setosa).
data(4.6,3.6,1.0,0.2,setosa).
data(5.1,3.3,1.7,0.5,setosa).
data(4.8,3.4,1.9,0.2,setosa).
data(5.0,3.0,1.6,0.2,setosa).
data(5.0,3.4,1.6,0.4,setosa).
data(5.2,3.5,1.5,0.2,setosa).
data(5.2,3.4,1.4,0.2,setosa).
data(4.7,3.2,1.6,0.2,setosa).
data(4.8,3.1,1.6,0.2,setosa).
data(5.4,3.4,1.5,0.4,setosa).
data(5.2,4.1,1.5,0.1,setosa).
data(5.5,4.2,1.4,0.2,setosa).
data(4.9,3.1,1.5,0.1,setosa).
data(5.0,3.2,1.2,0.2,setosa).
data(5.5,3.5,1.3,0.2,setosa).
data(4.9,3.1,1.5,0.1,setosa).
data(4.4,3.0,1.3,0.2,setosa).
data(5.1,3.4,1.5,0.2,setosa).
data(5.0,3.5,1.3,0.3,setosa).
data(4.5,2.3,1.3,0.3,setosa).
data(4.4,3.2,1.3,0.2,setosa).
data(5.0,3.5,1.6,0.6,setosa).
data(5.1,3.8,1.9,0.4,setosa).
data(4.8,3.0,1.4,0.3,setosa).
data(5.1,3.8,1.6,0.2,setosa).
data(4.6,3.2,1.4,0.2,setosa).
data(5.3,3.7,1.5,0.2,setosa).
data(5.0,3.3,1.4,0.2,setosa).
data(7.0,3.2,4.7,1.4,versicolor).
data(6.4,3.2,4.5,1.5,versicolor).
data(6.9,3.1,4.9,1.5,versicolor).
data(5.5,2.3,4.0,1.3,versicolor).
data(6.5,2.8,4.6,1.5,versicolor).
data(5.7,2.8,4.5,1.3,versicolor).
data(6.3,3.3,4.7,1.6,versicolor).
data(4.9,2.4,3.3,1.0,versicolor).
data(6.6,2.9,4.6,1.3,versicolor).
data(5.2,2.7,3.9,1.4,versicolor).
data(5.0,2.0,3.5,1.0,versicolor).
data(5.9,3.0,4.2,1.5,versicolor).
data(6.0,2.2,4.0,1.0,versicolor).
data(6.1,2.9,4.7,1.4,versicolor).
data(5.6,2.9,3.6,1.3,versicolor).
data(6.7,3.1,4.4,1.4,versicolor).
data(5.6,3.0,4.5,1.5,versicolor).
data(5.8,2.7,4.1,1.0,versicolor).
data(6.2,2.2,4.5,1.5,versicolor).
data(5.6,2.5,3.9,1.1,versicolor).
data(5.9,3.2,4.8,1.8,versicolor).
data(6.1,2.8,4.0,1.3,versicolor).
data(6.3,2.5,4.9,1.5,versicolor).
data(6.1,2.8,4.7,1.2,versicolor).
data(6.4,2.9,4.3,1.3,versicolor).
data(6.6,3.0,4.4,1.4,versicolor).
data(6.8,2.8,4.8,1.4,versicolor).
data(6.7,3.0,5.0,1.7,versicolor).
data(6.0,2.9,4.5,1.5,versicolor).
data(5.7,2.6,3.5,1.0,versicolor).
data(5.5,2.4,3.8,1.1,versicolor).
data(5.5,2.4,3.7,1.0,versicolor).
data(5.8,2.7,3.9,1.2,versicolor).
data(6.0,2.7,5.1,1.6,versicolor).
data(5.4,3.0,4.5,1.5,versicolor).
data(6.0,3.4,4.5,1.6,versicolor).
data(6.7,3.1,4.7,1.5,versicolor).
data(6.3,2.3,4.4,1.3,versicolor).
data(5.6,3.0,4.1,1.3,versicolor).
data(5.5,2.5,4.0,1.3,versicolor).
data(5.5,2.6,4.4,1.2,versicolor).
data(6.1,3.0,4.6,1.4,versicolor).
data(5.8,2.6,4.0,1.2,versicolor).
data(5.0,2.3,3.3,1.0,versicolor).
data(5.6,2.7,4.2,1.3,versicolor).
data(5.7,3.0,4.2,1.2,versicolor).
data(5.7,2.9,4.2,1.3,versicolor).
data(6.2,2.9,4.3,1.3,versicolor).
data(5.1,2.5,3.0,1.1,versicolor).
data(5.7,2.8,4.1,1.3,versicolor).
data(6.3,3.3,6.0,2.5,virginica).
data(5.8,2.7,5.1,1.9,virginica).
data(7.1,3.0,5.9,2.1,virginica).
data(6.3,2.9,5.6,1.8,virginica).
data(6.5,3.0,5.8,2.2,virginica).
data(7.6,3.0,6.6,2.1,virginica).
data(4.9,2.5,4.5,1.7,virginica).
data(7.3,2.9,6.3,1.8,virginica).
data(6.7,2.5,5.8,1.8,virginica).
data(7.2,3.6,6.1,2.5,virginica).
data(6.5,3.2,5.1,2.0,virginica).
data(6.4,2.7,5.3,1.9,virginica).
data(6.8,3.0,5.5,2.1,virginica).
data(5.7,2.5,5.0,2.0,virginica).
data(5.8,2.8,5.1,2.4,virginica).
data(6.4,3.2,5.3,2.3,virginica).
data(6.5,3.0,5.5,1.8,virginica).
data(7.7,3.8,6.7,2.2,virginica).
data(7.7,2.6,6.9,2.3,virginica).
data(6.0,2.2,5.0,1.5,virginica).
data(6.9,3.2,5.7,2.3,virginica).
data(5.6,2.8,4.9,2.0,virginica).
data(7.7,2.8,6.7,2.0,virginica).
data(6.3,2.7,4.9,1.8,virginica).
data(6.7,3.3,5.7,2.1,virginica).
data(7.2,3.2,6.0,1.8,virginica).
data(6.2,2.8,4.8,1.8,virginica).
data(6.1,3.0,4.9,1.8,virginica).
data(6.4,2.8,5.6,2.1,virginica).
data(7.2,3.0,5.8,1.6,virginica).
data(7.4,2.8,6.1,1.9,virginica).
data(7.9,3.8,6.4,2.0,virginica).
data(6.4,2.8,5.6,2.2,virginica).
data(6.3,2.8,5.1,1.5,virginica).
data(6.1,2.6,5.6,1.4,virginica).
data(7.7,3.0,6.1,2.3,virginica).
data(6.3,3.4,5.6,2.4,virginica).
data(6.4,3.1,5.5,1.8,virginica).
data(6.0,3.0,4.8,1.8,virginica).
data(6.9,3.1,5.4,2.1,virginica).
data(6.7,3.1,5.6,2.4,virginica).
data(6.9,3.1,5.1,2.3,virginica).
data(5.8,2.7,5.1,1.9,virginica).
data(6.8,3.2,5.9,2.3,virginica).
data(6.7,3.3,5.7,2.5,virginica).
data(6.7,3.0,5.2,2.3,virginica).
data(6.3,2.5,5.0,1.9,virginica).
data(6.5,3.0,5.2,2.0,virginica).
data(6.2,3.4,5.4,2.3,virginica).
data(5.9,3.0,5.1,1.8,virginica).

sample_clusters(True,Prior,Post):-
  findall([A,B,C,D,Type],data(A,B,C,D,Type),LA),
  pca(LA,True).


pca(LA,P):-
  maplist(add_cat,LA,LCat,L),
  list_to_set(LCat,Cats),
  data<- L,
  dpca<- prcomp(data),
  PC <- dpca,
  member(x=Data,PC),
  %findall(X-Y,(member(x=Data,PC),member([X,Y,_,_],Data)),LB),
  maplist(add_cat,DC,LCat,Data),
  maplist(separate(DC),Cats,Mat1),
%  maplist(xy,Setosa,XSet,YSet),
%  maplist(xy,Versicolor,XVer,YVer),
%  maplist(xy,Virginica,XVir,YVir),
  maplist(keep2,Mat1,Mat2),
  maplist(axis,Cats,Axis,LAxis),
  dict_create(Ax,_,LAxis),
  maplist(tocol,Mat2,Axis,ColData),
  append(ColData,Cols),
  P = c3{data:_{
    xs:Ax,
%x: setosa_x,
     columns:
                  Cols
            ,
%       ["versicolor_x"|XVer],[ver|YVer],
%       ["virginica_x"|XVir],[vir|YVir]],
     type:scatter},
        axis:_{
        x:_{
            tick:_{
                fit: false
            }
        }}
%          axis:_{ x:_{ tick:_{values:Tick}}},
%    format: 'function (x) { return x.toFixed(2);}' ,
%           fit: true,culling:_{max:7} }} },
%          bar:_{
%            width:_{ ratio: 1.0 }}, 
%            legend:_{show: false}
}.

tocol(Data,[X,Y],[[X|DX],[Y|DY]]):-
  maplist(xy,Data,DX,DY).

ab([X,Y,_,_,_],[X,Y]).
xy([X,Y],X,Y).
add_cat([X,Y,Z,W,C],C,[X,Y,Z,W]).
set([_,_,_,_,setosa]).
cat(Cat,[_,_,_,_,Cat]).

group([A,B],[C,D],[E,F],g(A,B,C,D,E,F)).

axis(N,[NX,N],N-NX):-
  atom_concat(x,N,NX).

keep2(Quad,Cou):-
  maplist(ab,Quad,Cou).


separate(DC,Cat,DataClass):-
  include(cat(Cat),DC,DataClass).


/** <examples>
?- sample_clusters(True,Prior,Post).
?- mc_sample_arg_first(iris(0,X1,X2,X2,X4),10,(X1,X2,X3,X4),V).
?- pca(P).

*/
