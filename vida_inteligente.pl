% Librerias
:- use_module(library(csv)).
:- use_module(library(lists)).

% Hechos dinamicos
:- dynamic hecho/2.

% ======= Menú principal =======

inicio :- 
    nl, writeln('Sistema Experto: Vida Inteligente'),
    writeln('1. Cargar hechos (desde planetas.csv)'),
    writeln('2. Imprimir hechos cargados'),
    writeln('3. Consultar inferencia (inferir/2)'),
    writeln('4. Demostrar inferencia (por_que/3)'),
    writeln('5. Salir del programa'),
    nl, write('Seleccione una opcion: '),
    read(Opcion),
    ejecutar(Opcion).

% Ejecutar la opcion seleccionada
ejecutar(1) :-
    cargar_datos, !, inicio.
ejecutar(2) :-
    imprimir_hechos, !, inicio.
ejecutar(3) :-
    consultar_inferencia, !, inicio.
ejecutar(4) :-
    consultar_demostracion, !, inicio.
ejecutar(5) :-
    writeln('Saliendo del programa...').
ejecutar(_) :-
    writeln('Opcion no valida. Vuelve a intentarlo'), !, inicio.

% ======= Carga de datos =======

% Cargar datos desde planetas.csv
cargar_datos :-
    writeln('Cargando los datos'),
    (exists_file('planetas.csv') -> 
        % Lee el CSV y crea los predicados en formato 'hecho(Planeta, Condicion)'
        csv_read_file('planetas.csv', [Header|Filas], [functor(hecho)]),
        % Limpia para evitar duplicados
        retractall(hecho(_,_)),
        % Agregamos los hechos a la base de datos
        maplist(assertz, Filas),
        writeln('Datos cargados exitosamente.'),
        write('Columnas: '), writeln(Header)
    ;   writeln('El archivo planetas.csv no existe. No se pudieron cargar los datos.')
    ).

% ======= Imprimir los hechos cargados =======

imprimir_hechos :-
    % findall(plantilla, objetivo, listaDeResulatdos), 
    % busca todos los hechos que coincidan con "objetivo" y 
    % los agrega a "listaDeResultados" en formato indicado por la "plantilla"
    (findall(hecho(Planeta, Condicion), hecho(Planeta, Condicion), ListaHechos),
        ( ListaHechos = [] ->
            writeln('No hay hechos cargados. Puede usar la opción 1 para cargar los hechos')
            ; maplist(writeln_hecho, ListaHechos)
        )
    ).

writeln_hecho(hecho(Planeta, Condicion)) :-
    format('  [~w] -> ~w~n', [Planeta, Condicion]).


% ======= Inferencia y reglas =======

% Definimos la inferencia como 'requisitos(Condicion, ListaDeRequisitos)'
requisitos(vida_basica, [tiene_atmosfera, tiene_agua_liquida, tiene_elementos_biogenicos]).
requisitos(vida_compleja, [vida_basica, tiene_evolucion_biologica, tiene_superficie_solida]).
requisitos(vida_inteligente, [vida_compleja, tiene_tecnologia]).
requisitos(habitable, [tiene_atmosfera, tiene_magnetosfera, tiene_gravedad_estable, tiene_ciclo_dia_noche]).
requisitos(fotosintesis_posible, [tiene_luz_solar]).
requisitos(civilizacion_avanzada, [vida_inteligente, habitable, fotosintesis_posible]).

% Explicación (por_que/3)

% Caso 1: La condición es una REGLA (tiene requisitos)
% Construye la explicación [inferido(Cond) | ExplicacionDeHijos]
por_que(Condicion, Planeta, [inferido(Condicion) | ExplicacionHijos]) :-
    requisitos(Condicion, ListaReqs),
    !,
    demostrar_lista(ListaReqs, Planeta, ExplicacionHijos).

% Caso 2: La condición es un HECHO BASE (no tiene requisitos)
% La explicación es simplemente el hecho base.
por_que(Condicion, Planeta, [hecho_base(Condicion)]) :-
    \+ requisitos(Condicion, _),
    hecho(Planeta, Condicion).

% Demostrar una lista de requisitos de forma recursiva
demostrar_lista([], _, []).
demostrar_lista([Req | RestoReqs], Planeta, ExplicacionTotal) :-
    por_que(Req, Planeta, ExplicacionReq),
    demostrar_lista(RestoReqs, Planeta, ExplicacionResto),
    append(ExplicacionReq, ExplicacionResto, ExplicacionTotal).

% Predicados de consulta (demostraciones)
% Verifica si una condición es verdadera (TRUE/FALSE o instancia variable).
% Simplemente llama a por_que/3 y descarta la explicación.
inferir(Condicion, Planeta) :-
    por_que(Condicion, Planeta, _Explicacion).


% ======= Predicados de consulta (Demostraciones) =======

% Arbol jerarquico de la demostracion
demostrar(Condicion, Planeta) :-
    format('--- Demostracion de [~w] para [~w] ---~n', [Condicion, Planeta]),
    ( por_que_jerarquico(Condicion, Planeta, 0) ->
        true
    ;   format('Fallo la demostracion para ~w en ~w~n', [Condicion, Planeta])
    ),
    writeln('--- Fin Demostracion ---').

% Variante jerárquica de por_que/3 que imprime con niveles
por_que_jerarquico(Condicion, Planeta, Nivel) :-
    requisitos(Condicion, ListaReqs), !,
    indent(Nivel), format('Inferido: ~w~n', [Condicion]),
    NivelSiguiente is Nivel + 2,
    imprimir_lista_jerarquica(ListaReqs, Planeta, NivelSiguiente).

por_que_jerarquico(Condicion, Planeta, Nivel) :-
    hecho(Planeta, Condicion),
    indent(Nivel), format('Hecho_Base: ~w~n', [Condicion]).

% Recorre cada requisito de forma recursiva
imprimir_lista_jerarquica([], _, _).
imprimir_lista_jerarquica([Req | Resto], Planeta, Nivel) :-
    por_que_jerarquico(Req, Planeta, Nivel),
    imprimir_lista_jerarquica(Resto, Planeta, Nivel).

% Imprime indentación (espacios)
indent(Nivel) :-
    Espacios is Nivel,
    forall(between(1, Espacios, _), write(' ')).


% Submenú para consultar inferencia (inferir/2)
consultar_inferencia :-
    nl, writeln('--- Consultar Inferencia (inferir/2) ---'),
    writeln('Ej: Condicion = vida_inteligente, Planeta = P.'),
    writeln('Ej: Condicion = habitable, Planeta = tierra.'),
    write('Ingrese Condicion: '), read(Condicion),
    write('Ingrese Planeta (o P para variable): '), read(Planeta),
    ( var(Planeta) -> % Si el planeta es una variable (P)
        writeln('Buscando planetas que cumplen...'),
        % setof/3 es como findall/3 pero ordena y elimina duplicados
        ( setof(P, inferir(Condicion, P), Soluciones) ->
            format('Planetas con ~w:~n', [Condicion]),
            maplist(writeln_sol, Soluciones)
        ;    format('No se encontraron planetas con ~w.~n', [Condicion])
        )
    ; % Si el planeta está instanciado (ej: tierra)
        ( inferir(Condicion, Planeta) ->
            format('CONFIRMADO: Si, ~w cumple con ~w.~n', [Planeta, Condicion])
        ; format('NEGATIVO: No, ~w no cumple con ~w.~n', [Planeta, Condicion])
        )
    ).
writeln_sol(S) :- format('  - ~w~n', [S]).

% Submenú para consultar demostrar inferencia (demostrar/2)
consultar_demostracion :-
    nl, writeln('--- Demostrar Inferencia (por_que/3) ---'),
    writeln('Ej: Condicion = vida_inteligente, Planeta = tierra.'),
    write('Ingrese Condicion: '), read(Condicion),
    write('Ingrese Planeta (debe ser especifico): '), read(Planeta),
    ( var(Planeta) ->
        writeln('Error: El planeta debe ser especifico (no una variable).')
    ; % Llama a demostrar/2 que ya imprime el resultado
      demostrar(Condicion, Planeta)
    ).

:- initialization(inicio, main).