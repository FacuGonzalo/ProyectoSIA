:- consult(minaExample).
:- use_module(library(pairs)).

estaLibre(Destino):- 
    not(estaEn([r,_],Destino)),                                                                  
    not(estaEn([p,_,_],Destino)),                                                                
    not(estaEn([v,_,_],Destino)).                                                                

/* MOVIMIENTOS */

nuevaCelda([X,Y], [XNuevo,YNuevo],caminar,Dir):-
    (Dir == n, XNuevo is (X-1), YNuevo is Y);                                                    
    (Dir == s, XNuevo is (X+1), YNuevo is Y);                                                    
    (Dir == o, YNuevo is (Y-1), XNuevo is X);                                                    
    (Dir == e, YNuevo is (Y+1), XNuevo is X).                                                    

/* ----------------------------------------------------- ACCIONES DEL MINERO ----------------------------------------------------------------------------- */

/* CAMINAR */
accion([Actual, Dir, ListaPosesiones,ColocacionCargaPendiente],[Destino,Dir,ListaPosesiones,ColocacionCargaPendiente],caminar,Costo):-
    nuevaCelda(Actual,Destino,caminar,Dir),                                                                                                 
    (estaLibre(Destino);                                                                                                                     
     estaEn([r,R2],Destino), member([l,L1],ListaPosesiones), abreReja([l,L1],[r,R2])                                                         
    ),
    not(estaEn([p,_,_], Destino)),
    not(estaEn([v,_,_], Destino)),
    celda(Destino,TipoSuelo),                                                                                                                
    ((TipoSuelo == firme, Costo is 2); (TipoSuelo == resbaladizo, Costo is 3)).                                                             

/* SALTAR */
accion([Actual,Dir,ListaPosesiones,ColocacionCargaPendiente],[Destino2,Dir,ListaPosesiones,ColocacionCargaPendiente],saltar([v,Valla,AlturaValla]),Costo):-
    nuevaCelda(Actual,Destino1, caminar, Dir),
    estaEn([v, Valla, AlturaValla], Destino1),
    AlturaValla < 4,
    not(estaEn([p,_,_], Destino1)),
    not(estaEn([r,_], Destino1)),
    nuevaCelda(Destino1,Destino2, caminar, Dir),
    not(estaEn([v,_,_], Destino2)),
    not(estaEn([p,_,_], Destino2)),
    celda(Destino2,TipoSuelo),                                                                                                               
    ((TipoSuelo == firme, Costo is 3); (TipoSuelo == resbaladizo, Costo is 4)).
   

/* ROTAR */
accion([Actual, Dir, ListaPosesiones,ColocacionCargaPendiente], [Actual, DirNueva, ListaPosesiones,ColocacionCargaPendiente],rotar(DirNueva),Costo):-
    member(DirNueva, [n,s,e,o]),
    (Dir \= DirNueva,
     Dir == n, DirNueva == e, Costo is 1;                                                                              
     Dir == n, DirNueva == o, Costo is 1;                                                                              
     Dir == n, DirNueva == s, Costo is 2;                                                                              
     Dir == s, DirNueva == e, Costo is 1;                                                                              
     Dir == s, DirNueva == o, Costo is 1;                                                                              
     Dir == s, DirNueva == n, Costo is 2;                                                                              
     Dir == e, DirNueva == n, Costo is 1;                                                                              
     Dir == e, DirNueva == s, Costo is 1;                                                                              
     Dir == e, DirNueva == o, Costo is 2;                                                                              
     Dir == o, DirNueva == n, Costo is 1;                                                                              
     Dir == o, DirNueva == s, Costo is 1;                                                                              
     Dir == o, DirNueva == e, Costo is 2                                                                               
    ).

/* JUNTAR LLAVE */
accion([Actual,Dir,ListaPosesiones,ColocacionCargaPendiente],[Actual,Dir,ListaPosesionesActualizada,ColocacionCargaPendiente],juntar_llave([l,L1]),Costo):-
    estaEn([l,L1],Actual),                                                                                                                                      
    not(member([l,L1],ListaPosesiones)),                                                                                                                        
    append(ListaPosesiones,[[l,L1]],ListaPosesionesActualizada),                                                                                                  
    Costo is 1.                                                                                                                                                 

/* JUNTAR CARGA */
accion([Actual,Dir,ListaPosesiones,_],[Actual,Dir,ListaPosesionesActualizada,si],juntar_carga([c,c1]),Costo):-
    estaEn([c,c1],Actual),                                                                                            
    not(member([c,c1],ListaPosesiones)),                                                                              
    append(ListaPosesiones,[[c,c1]],ListaPosesionesActualizada),                                                        
    Costo is 3.                                                                                                       

/* DEJAR CARGA */
accion([Actual,Dir,ListaPosesiones,si],[Actual,Dir,ListaPosesiones,no],dejar_carga([c,c1]),Costo):-
    ubicacionCarga(Actual),                                                                                          
    member([c,c1],ListaPosesiones),                                                                                                                                                         
    Costo is 1.                                                                                                       

/* JUNTAR DETONADOR */
accion([Actual,Dir,ListaPosesiones,ColocacionCargaPendiente],[Actual,Dir,ListaPosesionesActualizada,ColocacionCargaPendiente],juntar_detonador([d,d1,no]),Costo):-
    estaEn([d,d1,no],Actual),                                                                                                                                         
    not(member([d,d1,no],ListaPosesiones)),                                                                                                                           
    append(ListaPosesiones,[[d,d1,no]],ListaPosesionesActualizada),                                                                                                       
    Costo is 2.                                                                                                                                                      

/* DETONAR */
accion([Actual,Dir,ListaPosesiones,no],[Actual,Dir,[[d,d1,si] | ListaPosesiones],no],detonar([d,d1,si]), Costo):-
    Detonador = [d,d1,no],
    member([c,c1], ListaPosesiones),
    member(Detonador,ListaPosesiones),  
    not(member([d,d1,si], ListaPosesiones)),
    sitioDetonacion(Actual),                
    Costo is 1.                             

/* ----------------------------------------------------- HEURISTICAS ---------------------------------------------------------------------------------- */
distanciaManhattan([X,Y], [Xs,Ys], Valor):-
    Valor is abs(X-Xs) + abs(Y-Ys).

calcularDistancias(_, [], []).

calcularDistancias(PosActual, [Meta | ListaMetas], [Distancia-Meta | RestoMetas]):-
    distanciaManhattan(PosActual, Meta, Distancia),
    calcularDistancias(PosActual, ListaMetas, RestoMetas).

ordenar([X,Y], ListaMetas, ListaMetasOrdenadas):-
    calcularDistancias([X,Y], ListaMetas, ListaMetasAux),
    keysort(ListaMetasAux, ListaMetasOrdenadas).

/* Caso: juntar_carga, juntar_detonador, dejar_carga, detonar */
heuristica(Estado, Valor):-
    Estado = [[X,Y], _, ListaPosesiones, _],
    not(member([c,c1], ListaPosesiones)),
    not(member([d,d1,no], ListaPosesiones)),
    
    estaEn([c,c1], [Xc,Yc]),
    calcularDistancias([X,Y], [[Xc,Yc]], [DistanciaCarga-_|_]),

    estaEn([d,d1,no], [Xd,Yd]),
    calcularDistancias([Xc,Yc], [[Xd,Yd]], [DistanciaDetonador-_|_]),

    ubicacionCarga([Xuc, Yuc]),
    calcularDistancias([Xd,Yd], [[Xuc, Yuc]], [DistanciaUbicacionCarga-_|_]),

    findall([Xm, Ym], sitioDetonacion([Xm,Ym]), ListaSitiosDetonacion),
    ordenar([Xuc,Yuc], ListaSitiosDetonacion, [DistanciaSitioDetonacion-_|_]),

    Valor is DistanciaCarga + DistanciaDetonador + DistanciaUbicacionCarga + DistanciaSitioDetonacion.

/* Caso: juntar_detonador, juntar_carga, dejar_carga, detonar */
heuristica(Estado, Valor):-
    Estado = [[X,Y], _, ListaPosesiones, _],
    not(member([c,c1], ListaPosesiones)),
    not(member([d,d1,no], ListaPosesiones)),

    estaEn([d,d1,no], [Xd,Yd]),
    calcularDistancias([X,Y], [[Xd,Yd]], [DistanciaDetonador-_|_]),

    estaEn([c,c1], [Xc,Yc]),
    calcularDistancias([Xd,Yd], [[Xc,Yc]], [DistanciaCarga-_|_]),

    ubicacionCarga([Xuc, Yuc]),
    calcularDistancias([Xd,Yd], [[Xuc, Yuc]], [DistanciaUbicacionCarga-_|_]),

    findall([Xm, Ym], sitioDetonacion([Xm,Ym]), ListaSitiosDetonacion),
    ordenar([Xuc,Yuc], ListaSitiosDetonacion, [DistanciaSitioDetonacion-_|_]),

    Valor is DistanciaCarga + DistanciaDetonador + DistanciaUbicacionCarga + DistanciaSitioDetonacion.

/* Caso: juntar_carga, dejar_carga, juntar_detonador, detonar */
heuristica(Estado, Valor):-
    Estado = [[X,Y], _, ListaPosesiones, _],
    not(member([c,c1], ListaPosesiones)),
    not(member([d,d1,no], ListaPosesiones)),

    estaEn([c,c1], [Xc,Yc]),
    calcularDistancias([X,Y], [[Xc,Yc]], [DistanciaCarga-_|_]),

    ubicacionCarga([Xuc, Yuc]),
    calcularDistancias([Xc,Yc], [[Xuc, Yuc]], [DistanciaUbicacionCarga-_|_]),

    estaEn([d,d1,no], [Xd,Yd]),
    calcularDistancias([Xuc,Yuc], [[Xd,Yd]], [DistanciaDetonador-_|_]),

    findall([Xm, Ym], sitioDetonacion([Xm,Ym]), ListaSitiosDetonacion),
    ordenar([Xd,Yd], ListaSitiosDetonacion, [DistanciaSitioDetonacion-_|_]),

    Valor is DistanciaCarga + DistanciaDetonador + DistanciaUbicacionCarga + DistanciaSitioDetonacion.

/* Caso: juntar_detonador, dejar_carga, detonar */
heuristica(Estado, Valor):-
    Estado = [[X,Y], _, ListaPosesiones, si],
    member([c,c1], ListaPosesiones),
    not(member([d,d1,no], ListaPosesiones)),

    estaEn([d,d1,no], [Xd,Yd]),
    calcularDistancias([X,Y], [[Xd,Yd]], [DistanciaDetonador-_|_]),

    ubicacionCarga([Xuc, Yuc]),
    calcularDistancias([Xd,Yd], [[Xuc, Yuc]], [DistanciaUbicacionCarga-_|_]),

    findall([Xm, Ym], sitioDetonacion([Xm,Ym]), ListaSitiosDetonacion),
    ordenar([Xuc,Yuc], ListaSitiosDetonacion, [DistanciaSitioDetonacion-_|_]),

    Valor is DistanciaDetonador + DistanciaUbicacionCarga + DistanciaSitioDetonacion.

/* Caso: dejar_carga, juntar_detonador, detonar */
heuristica(Estado, Valor):-
    Estado = [[X,Y], _, ListaPosesiones, si],
    member([c,c1], ListaPosesiones),
    not(member([d,d1,no], ListaPosesiones)),

    ubicacionCarga([Xuc, Yuc]),
    calcularDistancias([X,Y], [[Xuc, Yuc]], [DistanciaUbicacionCarga-_|_]),

    estaEn([d,d1,no], [Xd,Yd]),
    calcularDistancias([Xuc,Yuc], [[Xd,Yd]], [DistanciaDetonador-_|_]),

    findall([Xm, Ym], sitioDetonacion([Xm,Ym]), ListaSitiosDetonacion),
    ordenar([Xuc,Yuc], ListaSitiosDetonacion, [DistanciaSitioDetonacion-_|_]),

    Valor is DistanciaDetonador + DistanciaUbicacionCarga + DistanciaSitioDetonacion.

/* Caso: juntar_carga, dejar_carga, detonar */
heuristica(Estado, Valor):-
    Estado = [[X,Y], _, ListaPosesiones, _],
    not(member([c,c1], ListaPosesiones)),
    member([d,d1,no], ListaPosesiones),

    estaEn([c,c1], [Xc,Yc]),
    calcularDistancias([X,Y], [[Xc,Yc]], [DistanciaCarga-_|_]),

    ubicacionCarga([Xuc, Yuc]),
    calcularDistancias([Xc,Yc], [[Xuc, Yuc]], [DistanciaUbicacionCarga-_|_]),

    findall([Xm, Ym], sitioDetonacion([Xm,Ym]), ListaSitiosDetonacion),
    ordenar([Xuc,Yuc], ListaSitiosDetonacion, [DistanciaSitioDetonacion-_|_]),

    Valor is DistanciaCarga + DistanciaUbicacionCarga + DistanciaSitioDetonacion.

/* Caso: juntar_detonador, detonar */
heuristica(Estado, Valor):-
    Estado = [[X,Y], _, ListaPosesiones, no],
    member([c,c1], ListaPosesiones),
    not(member([d,d1,no], ListaPosesiones)),

    estaEn([d,d1,no], [Xd,Yd]),
    calcularDistancias([X,Y], [[Xd,Yd]], [DistanciaDetonador-_|_]),

    findall([Xm, Ym], sitioDetonacion([Xm,Ym]), ListaSitiosDetonacion),
    ordenar([Xd,Yd], ListaSitiosDetonacion, [DistanciaSitioDetonacion-_|_]),

    Valor is DistanciaDetonador + DistanciaSitioDetonacion.

/* Caso: dejar_carga, detonar */
heuristica(Estado, Valor):-
    Estado = [[X,Y], _, ListaPosesiones, si],
    member([c,c1], ListaPosesiones),
    member([d,d1,no], ListaPosesiones),

    ubicacionCarga([Xuc, Yuc]),
    calcularDistancias([X,Y], [[Xuc, Yuc]], [DistanciaUbicacionCarga-_|_]),

    findall([Xm, Ym], sitioDetonacion([Xm,Ym]), ListaSitiosDetonacion),
    ordenar([Xuc,Yuc], ListaSitiosDetonacion, [DistanciaSitioDetonacion-_|_]),

    Valor is DistanciaUbicacionCarga + DistanciaSitioDetonacion.

/* Caso: detonar */
heuristica(Estado, Valor):-
    Estado = [[X,Y], _, ListaPosesiones, no],
    member([c,c1], ListaPosesiones),
    member([d,d1,no], ListaPosesiones),

    findall([Xm, Ym], sitioDetonacion([Xm,Ym]), ListaSitiosDetonacion),
    ordenar([X,Y], ListaSitiosDetonacion, [DistanciaSitioDetonacion-_|_]),

    Valor is DistanciaSitioDetonacion.

meta_final(Estado):-
    Estado = [[X,Y], _, ListaPosesiones, no],
    member([c,c1], ListaPosesiones),
    member([d,d1,si], ListaPosesiones),
    sitioDetonacion([X,Y]).

/*------------------------------------------------------------ BUSQUEDA ---------------------------------------------------------------------*/

buscar_plan(EstadoInicial, Plan, Destino, Costo):-
    buscarEn([nodo(EstadoInicial, [], 0)], [], nodo(EstadoFinal, Camino, Costo)),
    EstadoFinal = [Destino,_,_,_],
    reverse(Camino, Plan), !.

buscar_plan(_,_,_,_):-
    writeln('No se ha podido encontrar ningun plan :('),
    fail.

seleccionar(Nodo, [Nodo|RestoLista], RestoLista).

buscarEn(Frontera, _Vis, Nodo):- 
    seleccionar(Nodo,Frontera,_FronteraSinNodo),
    Nodo = nodo(Estado, _CaminoE, _Costo),
    meta_final(Estado), !.

buscarEn(Frontera, Visitados, Camino):-
    seleccionar(Nodo,Frontera,FronteraSinNodo),
    vecinos(Nodo, FronteraSinNodo, NuevaFrontera1, Visitados, NuevosVisitados, Vecinos),
    agregar(Vecinos, NuevaFrontera1, NuevaFrontera2),
    buscarEn(NuevaFrontera2,[Nodo | NuevosVisitados],Camino).

vecinos(Nodo, Frontera, NuevaFrontera, Visitados, NuevosVisitados, NuevosVecinos):-
    Nodo = nodo(Estado,Camino,Costo),
    findall(nodo(ESuc,[Accion | Camino], CostoN), (accion(Estado,ESuc,Accion,CostoOp), CostoN is Costo + CostoOp),Vecinos),
    recorrerFronteraVisitados(Vecinos, NuevosVecinos, Frontera, NuevaFrontera, Visitados, NuevosVisitados).

recorrerFronteraVisitados([], [], Frontera, Frontera, Visitados, Visitados).

/* El nuevo vecino no está en la frontera, tampoco en visitados */
recorrerFronteraVisitados([Nodo | Vecinos], [Nodo | NuevosVecinos], Frontera, NuevaFrontera, Visitados, NuevosVisitados):-
    Nodo = nodo(Estado, _, _),
    not(member(nodo(Estado, _, _), Frontera)),
    not(member(nodo(Estado, _, _), Visitados)),
    recorrerFronteraVisitados(Vecinos, NuevosVecinos, Frontera, NuevaFrontera, Visitados, NuevosVisitados).

/* El nuevo vecino esta en la frontera pero con menor costo, se reemplaza */
recorrerFronteraVisitados([Nodo | Vecinos], NuevosVecinos, Frontera, NuevaFrontera, Visitados, NuevosVisitados):-
    Nodo = nodo(Estado, _Camino, Costo),
    member(nodo(Estado, _, CostoF), Frontera),
    Costo < CostoF,
    delete(Frontera, nodo(Estado, _, CostoF), NewFrontera1),
    append(NewFrontera1, [Nodo], NewFrontera2),
    recorrerFronteraVisitados(Vecinos, NuevosVecinos, NewFrontera2, NuevaFrontera, Visitados, NuevosVisitados).

/* El nuevo vecino esta en la frontera pero con mayor costo, no se hace nada */
recorrerFronteraVisitados([Nodo | Vecinos], NuevosVecinos, Frontera, NuevaFrontera, Visitados, NuevosVisitados):-
    Nodo = nodo(Estado, _Camino, Costo),
    member(nodo(Estado, _, CostoF), Frontera),
    Costo >= CostoF,
    recorrerFronteraVisitados(Vecinos, NuevosVecinos, Frontera, NuevaFrontera, Visitados, NuevosVisitados).

/* El nuevo vecino tiene menor costo que el de visitados, se elimina de visitados y se agrega a la frontera */
recorrerFronteraVisitados([Nodo | Vecinos], NuevosVecinos, Frontera, NuevaFrontera, Visitados, NuevosVisitados):-
    Nodo = nodo(Estado, _Camino, Costo),
    member(nodo(Estado, _, CostoV), Visitados),
    Costo < CostoV,
    delete(Visitados, nodo(Estado, _, CostoV), NewVisitados),
    append(Frontera, [Nodo], NewFrontera),
    recorrerFronteraVisitados(Vecinos, NuevosVecinos, NewFrontera, NuevaFrontera, NewVisitados, NuevosVisitados).

/* El nuevo nodo esta en los visitados pero tiene mayor costo, no se hace nada */
recorrerFronteraVisitados([Nodo | Vecinos], NuevosVecinos, Frontera, NuevaFrontera, Visitados, NuevosVisitados):-
    Nodo = nodo(Estado, _Camino, Costo),
    member(nodo(Estado, _, CostoV), Visitados),
    Costo >= CostoV,
    recorrerFronteraVisitados(Vecinos, NuevosVecinos, Frontera, NuevaFrontera, Visitados, NuevosVisitados).

agregar(ViejaF,Vecinos,NuevaF):-
    append(Vecinos,ViejaF,Aux),
    ordenarH(Aux,NuevaF).

calcular_f([], []).
calcular_f([Nodo | Frontera], [F-Nodo | FronteraAux]):-
    Nodo = nodo(Estado, _, Costo),
    findall(Valor, heuristica(Estado, Valor), ListaValoresHeuristica),
    sort(0, @=<, ListaValoresHeuristica, [H | _]),
    F is Costo + 3*H,
    calcular_f(Frontera, FronteraAux).

ordenarH(Frontera, FronteraOrdenada):-
    calcular_f(Frontera, FronteraAux1),
    keysort(FronteraAux1, FronteraAux2),
    pairs_values(FronteraAux2, FronteraOrdenada).