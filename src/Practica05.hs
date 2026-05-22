module Practica05 where

import Terminos


-- ===================================================================================================================================


-- Aplicar una sustitucion a un termino
apsubT :: Term -> Subst -> Term
apsubT (Var x) s = buscaVar x s                                               --Si el término que nos pasaron es una variable, simplemente le pasamos el nombre de esa variable (x) y la sustitución (s) a nuestra función auxiliar buscaVar para que la busque y reemplace.
apsubT (Fun f args) s = Fun f (aplicarLista args s)                           --Si el término es una función, el nombre de la función se queda intacto. Cambian sus argumentos internos, usamos la función aplicarLista para pasarle la sustitución s a todos los args.

-- Funcion auxiliar para buscar el valor de una variable en la sustitución    
-- Si no la encuentra, devuelve la variable original intacta.
buscaVar :: Nombre -> Subst -> Term 
buscaVar x [] = Var x                                                         --Caso base. Busca x y si la lista esta vacia, devuelve Var x.
buscaVar x ((k, v):ss)                                                        --La separamos en su primer elemento (k, v) (donde k es el nombre de la variable a sustituir y v es el término de reemplazo) y el resto de la lista ss.
    | x == k    = v                                                           --Si el nombre de la variable que estoy buscando (x) es exactamente igual a la llave del primer par (k), entonces devuelve el término de reemplazo v.
    | otherwise = buscaVar x ss                                               --Si la condición anterior no se cumplió, entonces ignoramos este par y volvemos a llamar a buscaVar, buscando la misma x, pero ahora en el resto de la lista ss.

-- Funcion auxiliar para aplicar la sustitucion a una lista de terminos
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista [] _ = []                                                        --El caso base. Si le pasamos una lista vacía de términos, nos devuelve una lista vacía. 
aplicarLista (t:ts) s = apsubT t s : aplicarLista ts s                        --Separamos la lista de argumentos en el primer término (t) y todos los demás (ts). Le aplicamos la sustitución a t usando apsubT t s. Luego, concatenamos ese resultado con lo que salga de volver a llamar a aplicarLista sobre el resto de los términos (ts).



-- ===================================================================================================================================



-- Funcion que elimina los pares que son de la forma x=x
simpSus :: Subst -> Subst
simpSus [] = []                                                               --Una sustitución vacía ya está simplificada.
simpSus ((x, Var y):xs)                                                       --Analiza si el primer elemento es de la forma "sustituir la variable x por la variable y"
    | x == y    = simpSus xs                                                  --Si x es igual a y, ignoramos este par y seguimos simplificando el resto xs.
simpSus (p:xs) = p : simpSus xs                                               --Si es cualquier otro par válido, lo conservamos p y simplificamos el resto.

-- Funcion que calcula la composicion de dos sustituciones
compSus :: Subst -> Subst -> Subst
compSus s1 s2 = simpSus (s1_aplicada ++ s2_filtrada)                          --Une ambas listas ++ y elimina las trivialidades usando simpSus
  where
    s1_aplicada = aplicaASust s1 s2                                           --Aplica s2 a todos los términos que están dentro de s1
    dom_s1      = dominio s1                                                  --Obtiene cuáles son las variables originales que s1 está sustituyendo
    s2_filtrada = filtraDominio s2 dom_s1                                     --De s2, toma solo aquellas sustituciones cuyas variables no estén ya cubiertas por s1

-- Funcion auxiliar para aplicar s2 a los términos de s1
aplicaASust :: Subst -> Subst -> Subst
aplicaASust [] _ = []                                                               --El caso base. Si la primera sustitución está vacía [], devuelve una lista vacía.
aplicaASust ((x, t):xs) s2 = (x, apsubT t s2) : aplicaASust xs s2                   --Toma el primer par de s1, donde x es la variable y t es el término. Mantiene la misma variable x, pero al término t le aplica toda la sustitución s2 usando la función apsubT. Luego, une ese resultado y procesa el resto de la lista xs.

-- Funcion auxiliar para obtener el dominio (las variables) de una sustitución
dominio :: Subst -> [Nombre]
dominio [] = []                                                                     --Caso base: el dominio de una sustitución vacía es una lista vacía.
dominio ((x, _):xs) = x : dominio xs                                                --Toma el primer par (x, _). Nos quedamos solo con la variable x y la unimos (:) al resultado de calcular el dominio del resto de la lista xs.

-- Funcion auxiliar para saber si un elemento pertenece a una lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False                                                              --Caso base: si la lista está vacía, es imposible que el elemento esté ahí, así que devuelve False.
pertenece e (x:xs) = e == x || pertenece e xs                                       --Compara el elemento que buscamos e con el primer elemento de la lista x. Si son iguales, devuelve True. Si no lo son, el operador || hace que la función se vuelva a llamar para buscar e en el resto de la lista xs.

-- Funcion auxiliar para añadir elementos de s2 que no están en el dominio de s1
filtraDominio :: Subst -> [Nombre] -> Subst
filtraDominio [] _ = []                                                             --Caso base: si la sustitución a filtrar está vacía, devuelve vacío.
filtraDominio ((y, u):ys) dom                                                       --Analiza el primer par de s2 (la variable y y el término u) contra el dominio dom.
    | pertenece y dom = filtraDominio ys dom                                        --Si la variable 'y' pertenece al dominio de s1, ignoramos este par y seguimos filtrando el resto ys.
    | otherwise       = (y, u) : filtraDominio ys dom                               --Si no pertenece, la conservamos (y, u) y la unimos al filtrado del resto de la lista ys.




-- ===================================================================================================================================



-- Funcion que devuelve un umg de dos terminos, si es que lo hay
unifica :: Term -> Term -> [Subst]
unifica (Var x) t                                                                             --Primer caso: El lado izquierdo es una variable x y el derecho es cualquier término t.
    | Var x == t = [[]]                                                                       --Si la variable y el término son exactamente iguales, no hay que hacer ninguna sustitución. Devuelve una sustitución vacía dentro de la lista: [[]].
    | ocurre x t = []                                                                         --Si la variable x aparece dentro del término t, fallamos devolviendo [] porque crearía un bucle infinito.
    | otherwise  = [[(x, t)]]                                                                 --Si pasamos las dos pruebas anteriores, devolvemos la sustitución indicando que x debe ser reemplazada por t.
unifica t (Var x)                                                                             --Segundo caso: Exactamente lo mismo que el anterior, pero al revés. Sus guardas hacen exactamente las mismas validaciones.
    | Var x == t = [[]]
    | ocurre x t = []
    | otherwise  = [[(x, t)]]
unifica (Fun f args1) (Fun g args2)                                                           --Tercer caso: Ambos son funciones (o constantes, que son funciones sin argumentos).
    | f == g    = unificaListas args1 args2                                                   --Si el nombre de la función es el mismo, entonces el éxito depende de si podemos unificar sus argumentos internos. Llamamos a unificaListas.
    | otherwise = []                                                                          -- Falla porque los símbolos o aridades no coinciden

-- Funcion auxiliar: verifica si una variable aparece en un término
ocurre :: Nombre -> Term -> Bool
ocurre x (Var y) = x == y                                                                     --Si el término es otra variable y, simplemente verificamos si sus nombres son iguales.
ocurre x (Fun _ args) = ocurreEnLista x args                                                  --Si el término es una función, ignoramos su nombre (con el guion bajo _) y nos ponemos a buscar la variable dentro de su lista de argumentos usando ocurreEnLista.

-- Funcion auxiliar para verificar si una variable ocurre en una lista de términos
ocurreEnLista :: Nombre -> [Term] -> Bool
ocurreEnLista _ [] = False                                                                    --Si la lista está vacía, la variable no está ahí.
ocurreEnLista x (t:ts) = ocurre x t || ocurreEnLista x ts                                     --Revisa si la variable x ocurre en el primer término t. El operador || dice que si no está en t, siga buscando en el resto de la lista ts.

-- Funcion que devuelve un unificador de dos listas de términos funcionales
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas [] [] = [[]]                                                                    --Si ambas listas están vacías, terminamos sin necesidad de sustituciones.
unificaListas _ [] = []                                                                       --Si una lista tiene elementos y la otra está vacía, es un error estructural. Falla devolviendo [].
unificaListas [] _ = []
unificaListas (x:xs) (y:ys) =                                                                 --Compara el primer argumento de la izquierda x con el primero de la derecha y, separándolos del resto de las listas (xs y ys).
    case unifica x y of                                                                       --Intenta unificar ese primer par x e y.
        []   -> []                                                                            --Si ese primer par no se puede unificar, todo falla inmediatamente.
        [s1] -> case unificaListas (aplicarLista xs s1) (aplicarLista ys s1) of               --Si tuvo éxito y nos dio la sustitución s1, primero aplicamos esa sustitución s1 al resto de los elementos. Luego, intentamos unificar recursivamente esos restos ya actualizados.
                    []   -> []                                                                --Si la unificación del resto de la lista falla, todo falla.
                    [s2] -> [compSus s1 s2]                                                   --Si la unificación del resto de la lista tiene éxito dando s2, componemos la sustitución inicial s1 con la nueva s2 para entregar el resultado final.

-- Funcion que devuelve un umg de una lista de terminos, si es que la hay
unificaConj :: [Term] -> [Subst]
unificaConj [] = [[]]                                                                         --Si el conjunto está vacío o tiene un solo elemento, ya está unificado consigo mismo. Devuelve sin sustituciones.
unificaConj [_] = [[]]
unificaConj (t1:t2:ts) =                                                                      --Toma los dos primeros términos de la lista (t1 y t2) y deja el resto en ts.
    case unifica t1 t2 of                                                                     --Intenta unificar esos dos primeros términos.
        []   -> []                                                                            --Si fallan, el conjunto completo no es unificable.
        [s1] -> case unificaConj (aplicarLista (t2:ts) s1) of                                 --Si unifican dando s1, aplicamos esa sustitución a t2 y a todo el resto de la lista ts. Volvemos a llamar a unificaConj para continuar con el conjunto ya reducido y actualizado.
                    []   -> []                                                                --Si el resto del conjunto falla, todo falla.
                    [s2] -> [compSus s1 s2]                                                   --Si el resto del conjunto se logra unificar y nos da s2, componemos s1 y s2 y entregamos la sustitución final.
