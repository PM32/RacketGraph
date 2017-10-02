#lang racket

(define nodelist (list)) ; Lista de VERTICES
(define edgelist (list)) ; Lista de ARISTAS. Seran de la forma ([a,b] [c,d]) en donde [a,b] representa una arista entre el nodo a y b

;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCIONES CREAR GRAFO Y VERTICES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;graphCreate() -> Esta funcion pedire ingresar los vertices y los agrega en la lista de nodelist.
;Para terminar de ingresar vertices, introducir -1
(define (graphCreate)
  (define (addTo arglist readfunc)
    (if (equal? readfunc -1)
        (set! nodelist arglist)
        (addTo (append arglist (list readfunc)) (read))
        )
    )
  (addTo (list) (read))
)

;addNode() -> agrega un vertice a la lista.
(define (addNode v)
  (set! nodelist (append nodelist (list v)))
  #t
  )


;getNodeByPos() -> retorna el valor de la lista 'nodelist' en la posicion que recibe por parametro
(define (getNodeByPos pos)
  (list-ref nodelist pos)
  )

;totalNodes() -> retorna el tamaño de la lista 'nodelist'
(define (totalNodes)
  (length nodelist)
  )

;showNodes() -> recorre la lista de vertices y los imprime
(define (showNodes)
  (for ([i (totalNodes)])
    (display (getNodeByPos i))
    (newline)
    )
  )

;nodeExists() -> recorre la lista de vertices buscando el vertice 'v'. Si lo encuentra retorna true, sino un false
(define (nodeExists v)
  (define exists #f)
  (for ([i (totalNodes)])
    (cond
      [(= (getNodeByPos i) v) (set! exists #t)]
      )
    )
  exists
  )

;addEdge() -> Agrega una arista a la lista de aristas. La agrega inmediatamente validando la existencia de los vertices
(define (addEdge v1 v2)
  (cond
    [(not (nodeExists v1)) (error "Vertice v1 invalido")]
    [(not (nodeExists v2)) (error "Vertice v2 invalido")]
    )
  (set! edgelist (append edgelist (list (vector v1 v2) ) ))
  #t
  )

;deleteEdge() -> Borra un par de aristas del grafo
(define (deleteEdge a1 a2)
  (define tempedges (list))
  (for ([i (length edgelist)])
    (if (or  
         (and (= a1 (vector-ref (list-ref edgelist i) 0)) (= a2 (vector-ref (list-ref edgelist i) 1)) )
         (and (= a1 (vector-ref (list-ref edgelist i) 1)) (= a2 (vector-ref (list-ref edgelist i) 0)) )
         )
        (void)
        (set! tempedges (append tempedges (list (list-ref edgelist i))))
        )
    )
  (set! edgelist tempedges)
)

;totalEdges() -> Obtiene el total de aristas del grafo
(define (totalEdges)
  (length edgelist)
  )

;adyacentList() -> Devuelve una lista adyacente de nodos al nodo ingresado por parametro
(define (adyacentList node)
  (define adyacent (list))
  (define edgevec empty)
  (for ([i (totalEdges)])
    (set! edgevec (list-ref edgelist i))
    (cond
      [(= (vector-ref edgevec 0) node) (set! adyacent (append adyacent (list (vector-ref edgevec 1))))]
      [(= (vector-ref edgevec 1) node) (set! adyacent (append adyacent (list (vector-ref edgevec 0))))]
      )
    )
  adyacent
  )

;makeDFS() -> Recorre todos los nodos adyacentes de un nodo especificado por su posicion en la lista de vertices y devuelve una lista de nodos adyacentes
(define (makeDFS nodepos)
  (define explored (make-vector (totalNodes) #f))
  (define (DFS nodepos)
    (vector-set! explored nodepos #t)
    (define adyacents (adyacentList (getNodeByPos nodepos)))
    (define adyacentnodepos empty)
    (for ([w (length adyacents)])
      (set! adyacentnodepos (vector-member (list-ref adyacents w) (list->vector nodelist)))
      (cond [(equal? (vector-ref explored adyacentnodepos) #f) (DFS adyacentnodepos)])
      )
    #t
    )
  (DFS nodepos)
  explored
  )

;cycleGraph() -> Genera un grafo ciclico. NOTA: usar solo si no se han creado vertices
(define (cycleGraph numnodes)
  (cond
    [(<= numnodes 1) (error "El grafo debe ser de tamaño mayor a 1")]
    )
  (addNode 1)
  (for ([i (- numnodes 1)])
    (addNode (+ i 2))
    (addEdge (+ i 1) (+ i 2))
    )
  (addEdge 1 numnodes)
  #t
  )

;isConnected() -> Verifica si 2 nodos estan conectados
(define (isConnected orig dest)
  (define nodevector (list->vector nodelist))
  (define pos-orig (vector-member orig nodevector))
  (define pos-dest (vector-member dest nodevector))
  (cond
    [(equal? pos-orig #f) (error "Vertice inicial invalido")]
    [(equal? pos-dest #f) (error "Vertice final invalido")]
    )
  
  (define explored (makeDFS pos-orig))
  (vector-ref explored pos-dest)
  )

;graphClear() -> Limpia el grafo dejandolo sin vertices ni aristas
(define (graphClear)
  (set! nodelist (list))
  (set! edgelist (list))
  #t
  )