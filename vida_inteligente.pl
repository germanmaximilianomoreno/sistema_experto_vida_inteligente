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

% Explicación (por_que/3) - USADO POR 'inferir/2'

% Caso 1: La condición es una REGLA (tiene requisitos)
por_que(Condicion, Planeta, [inferido(Condicion) | ExplicacionHijos]) :-
    requisitos(Condicion, ListaReqs),
    demostrar_lista(ListaReqs, Planeta, ExplicacionHijos).

% Caso 2: La condición es un HECHO BASE (no tiene requisitos)
por_que(Condicion, Planeta, [hecho_base(Condicion)]) :-
    \+ requisitos(Condicion, _),
    hecho(Planeta, Condicion).

% Demostrar una lista de requisitos de forma recursiva
demostrar_lista([], _, []).
demostrar_lista([Req | RestoReqs], Planeta, ExplicacionTotal) :-
    por_que(Req, Planeta, ExplicacionReq),
    demostrar_lista(RestoReqs, Planeta, ExplicacionResto),
    append(ExplicacionReq, ExplicacionResto, ExplicacionTotal).

% Predicado 'inferir/2' (para Opción 3)
inferir(Condicion, Planeta) :-
    por_que(Condicion, Planeta, _Explicacion).


% ======= Predicados de consulta (Demostraciones) - USADO POR 'demostrar/2' =======

% Arbol jerarquico de la demostracion (para Opción 4)
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
    \+ requisitos(Condicion, _), % Guarda para evitar re-probar reglas
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


% ======= Submenús de Interfaz de Usuario =======

consultar_inferencia :-
    nl, writeln('--- Consultar Inferencia (inferir/2) ---'),
    writeln('Ej: Condicion = vida_inteligente, Planeta = P.'),
    writeln('Ej: Condicion = C (variable), Planeta = tierra.'),
    writeln('Ej: Condicion = habitable, Planeta = tierra.'),
    write('Ingrese Condicion: '), read(Condicion),
    write('Ingrese Planeta (o P para variable): '), read(Planeta),
    
    ( var(Planeta) -> 
        % CASO 1: Condicion=fija, Planeta=variable
        writeln('Buscando planetas que cumplen...'),
        ( setof(P, inferir(Condicion, P), Soluciones) ->
            format('Planetas con ~w:~n', [Condicion]),
            maplist(writeln_sol, Soluciones)
        ;   format('No se encontraron planetas con ~w.~n', [Condicion])
        )
        
    ; var(Condicion) ->
        % CASO 2: Condicion=variable, Planeta=fijo (¡NUEVO!)
        writeln('Buscando condiciones que cumplen...'),
        % setof/3 usa inferir/2 y el backtracking de Prolog para encontrar todo
        ( setof(C, inferir(C, Planeta), Soluciones) ->
            format('Condiciones cumplidas por ~w:~n', [Planeta]),
            maplist(writeln_sol, Soluciones)
        ;   format('No se encontraron condiciones cumplidas por ~w.~n', [Planeta])
        )

    ; % CASO 3: Ambos fijos
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
    writeln('Ej: Condicion = C (variable), Planeta = tierra.'),
    write('Ingrese Condicion: '), read(Condicion),
    write('Ingrese Planeta (debe ser especifico): '), read(Planeta),
    ( var(Planeta) ->
        writeln('Error: El Planeta debe ser especifico (no una variable).')
    ; var(Condicion) ->
        % Modo "Buscar Todos los Hechos Base"
        nl, format('--- Buscando todos los HECHOS BASE para [~w] ---~n', [Planeta]),
        (   hecho(Planeta, CondicionHecho), % Usamos una variable nueva
            format('  Hecho_Base: ~w~n', [CondicionHecho]),
            fail
        ;   true % Siempre tiene éxito al final
        ),
        format('--- Fin de la busqueda de hechos para [~w] ---~n', [Planeta])
    ; % Modo "Demostrar Uno"
      demostrar(Condicion, Planeta)
    ).

:- initialization(inicio, main).