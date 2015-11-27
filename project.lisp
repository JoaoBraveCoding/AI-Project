;;;; Grupo 50 ;;;; Aline Caliente n73151 ;;;; Jose Semedo n78294 ;;;; Joao Marcal n78471 ;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Primeira Entrega ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2.1.1 TIPO ACCAO

;;cria-accao: inteiro x array -> accao
(defun cria-accao (inteiro array)
 (cons inteiro array))

;;accao-coluna: accao -> inteiro
(defun accao-coluna (accao)
  (car accao))

;;accao-peca: accao -> array
(defun accao-peca (accao)
  (cdr accao))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2.1.2 TIPO TABULEIRO

;;cria-tabuleiro: -> tabuleiro
(defun cria-tabuleiro ()
  (make-array '(18 10)))

;;copia-tabuleiro: tabuleiro -> tabuleiro
(defun copia-tabuleiro (tabuleiro)
    (let ((tabuleiroNovo (cria-tabuleiro)))
          (loop for i from 0 to 17
                do(loop for j from 0 to 9
                        do(setf (aref tabuleiroNovo i j) (aref tabuleiro i j))))
    (return-from copia-tabuleiro tabuleiroNovo)))

;;tabuleiro-preenchido-p: tabuleiro x inteiro -> logico
(defun tabuleiro-preenchido-p (tabuleiro inteiroLinha inteiroColuna)
  (when (and (<= inteiroLinha 17) (<= inteiroColuna 9)) (aref tabuleiro inteiroLinha inteiroColuna)))

;;tabuleiro-altura-coluna: tabuleiro x inteiro -> inteiro
(defun tabuleiro-altura-coluna (tabuleiro inteiro)
  (loop for i from 0 to 17
        do(when (tabuleiro-preenchido-p tabuleiro (- 17 i) inteiro)
            (return-from tabuleiro-altura-coluna (- 18 i))))
  (return-from tabuleiro-altura-coluna 0))

;;tabuleiro-linha-completa-p: tabuleiro x inteiro -> logico
(defun tabuleiro-linha-completa-p (tabuleiro inteiro)
  (loop for i from 0 to 9
        do(cond ((eql (aref tabuleiro inteiro i) nil) (return-from tabuleiro-linha-completa-p nil))
                )
        )
  (return-from tabuleiro-linha-completa-p t)
  )

;;tabuleiro-preenche!: tabuleiro inteiro inteiro -> {}
(defun tabuleiro-preenche! (tabuleiro inteiroLinha inteiroColuna)
  (when (and (<= inteiroLinha 17) (<= inteiroColuna 9)) (setf (aref tabuleiro inteiroLinha inteiroColuna) t)))

;;tabuleiro-remove-linha!: tabuleiro inteiro -> {}
(defun tabuleiro-remove-linha! (tabuleiro inteiro)
  (loop for i from inteiro to 16
        do(loop for j from 0 to 9
                do(setf (aref tabuleiro i j) (aref tabuleiro (+ 1 i) j)))
   )
  (loop for i from 0 to 9
        do(setf (aref tabuleiro 17 i) nil)
   )
  )

;;tabuleiro-topo-preenchido-p: tabuleiro -> logico
(defun tabuleiro-topo-preenchido-p (tabuleiro)
  (loop for i from 0 to 9
        do(cond ((eql (aref tabuleiro 17 i) t) (return-from tabuleiro-topo-preenchido-p t))
                )
        )
  (return-from tabuleiro-topo-preenchido-p nil)
  )

;;tabuleiros-iguais-p: tabuleiro x tabuleiro -> logico
(defun tabuleiros-iguais-p (tabuleiro1 tabuleiro2)
  (loop for i from 0 to 9
        do(loop for j from 0 to 17
                do(cond((not(eql (aref tabuleiro1 j i) (aref tabuleiro2 j i))) (return-from tabuleiros-iguais-p nil))
                  )
        )
  )
  (return-from tabuleiros-iguais-p t)
  )

;;tabuleiro->array: tabuleiro -> array
(defun tabuleiro->array (tabuleiro)
  (let ((array (make-array '(18 10))))
    (setf array  (copia-tabuleiro tabuleiro))
    (return-from tabuleiro->array array)))

;;array->tabuleiro: array -> tabuleiro
(defun array->tabuleiro (array)
  (let ((tabuleiro (cria-tabuleiro)))
    (loop for i from 0 to 17
          do(loop for j from 0 to 9
                  do(cond((eql (aref array i j) t) (tabuleiro-preenche! tabuleiro i j)))))
  (return-from array->tabuleiro tabuleiro)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2.1.3 TIPO ESTADO

;;struct estado
(defstruct estado (pontos :0) pecas-por-colocar pecas-colocadas tabuleiro)

;;copia-estado: estado -> estado
(defun copia-estado (estado)
  (let ((estadoNovo (make-estado :pontos (estado-pontos estado) :pecas-por-colocar (make-list (length (estado-pecas-por-colocar estado))) :pecas-colocadas (make-list (length (estado-pecas-colocadas estado))) :tabuleiro (copia-tabuleiro (estado-tabuleiro estado)))))
    (loop for i from 0 to (1- (length (estado-pecas-por-colocar estado)))
          do(setf (nth i (estado-pecas-por-colocar estadoNovo)) (nth i (estado-pecas-por-colocar estado))))
    (loop for i from 0 to (1- (length (estado-pecas-colocadas estado)))
          do(setf (nth i (estado-pecas-colocadas estadoNovo)) (nth i (estado-pecas-colocadas estado))))
   (return-from copia-estado estadoNovo)))

;;estados-iguais-p: estado x estado -> logico
(defun estados-iguais-p (estado1 estado2)
  (cond ((not (eql (estado-pontos estado1) (estado-pontos estado2))) nil)
        ((not (tabuleiros-iguais-p (estado-tabuleiro estado1) (estado-tabuleiro estado2))) nil)
        ((not (equal (estado-pecas-por-colocar estado1) (estado-pecas-por-colocar estado2))) nil)
        ((not (equal (estado-pecas-colocadas estado1) (estado-pecas-colocadas estado2))) nil)
        (t t)))

;;estado-final-p: estado -> logico
(defun estado-final-p (estado)
  (cond ((tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) t)
        ((equal (estado-pecas-por-colocar estado) nil) t)
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2.1.4 TIPO PROBLEMA

;;struct problema
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2.2 Funcoes a implementar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2.2.1 Funcoes do problema de procura

;;solucao: estado -> logico
(defun solucao (estado)
  (cond ((tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) nil)
        ((equal (estado-pecas-por-colocar estado) nil) t)))

;;hash table para as posicoes das pecas
(defparameter orientations (make-hash-table :test 'equalp))
(setf (gethash "I" orientations) 2)
(setf (gethash "L" orientations) 4)
(setf (gethash "J" orientations) 4)
(setf (gethash "O" orientations) 1)
(setf (gethash "S" orientations) 2)
(setf (gethash "Z" orientations) 2)
(setf (gethash "T" orientations) 4)

;;tenta-colocar-peca: array array inteiro -> logico
(defun tenta-colocar-peca (peca tabuleiro coluna)
  (let ((alturaColuna  (tabuleiro-altura-coluna tabuleiro coluna)))
    (loop for i from 0 to (1- (array-dimension peca 0))
          do(loop for j from 0 to (1- (array-dimension peca 1))
                  do(when (and (tabuleiro-preenchido-p tabuleiro (+ i alturaColuna) (+ j coluna)) (aref peca i j))  (return-from tenta-colocar-peca nil))))
    (return-from tenta-colocar-peca t)))

;;accoes: estado -> lista de accoes
(defun accoes (estado)
  (let* ((listaDeAccoes (make-list 1))
        (peca (write-to-string (first (estado-pecas-por-colocar estado))))
        (nrOrient 0)
        (nomePeca (string "peca-"))
        (nomePecaSym nil))
    (when (estado-final-p estado) (return-from accoes nil))
    (setf nrOrient (gethash peca orientations))
    (setf nomePeca (concatenate 'string nomePeca peca))
    (loop for i from 0 to (1- nrOrient)
          do(progn (setf nomePecaSym (eval (read-from-string (concatenate 'string nomePeca (write-to-string i)))))
                   (loop for j from 0 to (- 10 (array-dimension nomePecaSym 1))
                         do(setf listaDeAccoes (append listaDeAccoes (list (cria-accao j  nomePecaSym)))))))
    (return-from accoes (rest listaDeAccoes))))

;;altura-colocar-peca: array array coluna -> inteiro
(defun altura-colocar-peca (peca tabuleiro coluna)
  (let ((alturaMaxima 0))
    (loop for i from 0 to (1- (array-dimension peca 1))
          do(when (> (tabuleiro-altura-coluna tabuleiro (+ coluna i)) alturaMaxima) (setf alturaMaxima (tabuleiro-altura-coluna tabuleiro (+ coluna i)))))
    (return-from altura-colocar-peca alturaMaxima)))

;;descer: peca altura coluna tabuleiro -> logico
(defun descer (peca altura coluna tabuleiro)
  (loop for i from 0 to (1- (array-dimension peca 0))
        do (loop for j from 0 to (1- (array-dimension peca 1))
                 do(when  (>= 17 (+ i altura)) (when (and (aref peca i j) (tabuleiro-preenchido-p tabuleiro (1- (+ i altura)) (+ j coluna))) (return-from descer nil)) )))
  (return-from descer t))

;;resultado: estado x accao -> estado
(defun resultado(estado accao)
  (let ((estadoNovo (copia-estado estado))
        (linha 0)
        (coluna (accao-coluna accao))
        (peca (accao-peca accao))
        (nrLinhasRem 0))
    (setf linha (altura-colocar-peca peca (estado-tabuleiro estado) coluna))
    (setf (estado-pecas-colocadas estadoNovo)
          (append (list (first (estado-pecas-por-colocar estado))) (estado-pecas-colocadas estado)))
    (setf (estado-pecas-por-colocar estadoNovo)
          (rest (estado-pecas-por-colocar estado)))
    (loop while (and (> linha 0) (descer peca linha coluna (estado-tabuleiro estado))) do
          (setf linha (- linha 1)))

    (loop for i from 0 to (1- (array-dimension peca 0))
          do (loop for j from 0 to (1- (array-dimension peca 1))
                   do(when (aref peca i j) (tabuleiro-preenche! (estado-tabuleiro estadoNovo) (+ i linha) (+ j coluna)))))
    (if (tabuleiro-topo-preenchido-p (estado-tabuleiro estadoNovo))
        (return-from resultado estadoNovo)
      (loop for i from 0 to 17
            do(when (tabuleiro-linha-completa-p (estado-tabuleiro estadoNovo) (- 17 i)) (progn (tabuleiro-remove-linha! (estado-tabuleiro estadoNovo) (- 17 i)) (setf nrLinhasRem (1+ nrLinhasRem))))))
    (cond ((eql nrLinhasRem 0 ) (setf (estado-pontos estadoNovo) (+ (estado-pontos estado) 0)))
          ((eql nrLinhasRem 1 ) (setf (estado-pontos estadoNovo) (+ (estado-pontos estado) 100)))
          ((eql nrLinhasRem 2 ) (setf (estado-pontos estadoNovo) (+ (estado-pontos estado) 300)))
          ((eql nrLinhasRem 3 ) (setf (estado-pontos estadoNovo) (+ (estado-pontos estado) 500)))
          (t  (setf (estado-pontos estadoNovo) (+ (estado-pontos estado) 800))))
    (return-from resultado estadoNovo)))

;;qualidade: estado->inteiro
(defun qualidade (estado)
  (* -1 (estado-pontos estado)))

;;custo-oportunidade: estado -> inteiro
(defun custo-oportunidade (estado)
  (let ((pontuacaoMaxima 0))
    (loop for i from 0 to (length (estado-pecas-colocadas estado))
          do(cond ((eql (nth i (estado-pecas-colocadas estado)) 'i) (setf pontuacaoMaxima (+ pontuacaoMaxima 800)))
                  ((eql (nth i (estado-pecas-colocadas estado)) 'j) (setf pontuacaoMaxima (+ pontuacaoMaxima 500)))
                  ((eql (nth i (estado-pecas-colocadas estado)) 'l) (setf pontuacaoMaxima (+ pontuacaoMaxima 500)))
                  ((eql (nth i (estado-pecas-colocadas estado)) 's) (setf pontuacaoMaxima (+ pontuacaoMaxima 300)))
                  ((eql (nth i (estado-pecas-colocadas estado)) 'z) (setf pontuacaoMaxima (+ pontuacaoMaxima 300)))
                  ((eql (nth i (estado-pecas-colocadas estado)) 't) (setf pontuacaoMaxima (+ pontuacaoMaxima 300)))
                  ((eql (nth i (estado-pecas-colocadas estado)) 'o) (setf pontuacaoMaxima (+ pontuacaoMaxima 300)))))
    (return-from custo-oportunidade (- pontuacaoMaxima (estado-pontos estado)))))

;;remove-de-lista: lista x inteiro -> lista
(defun remove-de-lista (lista inteiro)
  (let ((contador 0)
        (new-lista ()))
    (labels ((remove-de-lista-aux (lista-in)
                                  (cond ((= contador inteiro) (setf new-lista (append new-lista (rest lista-in))))
                                        (t (progn (setf new-lista (append new-lista (list (first lista-in))))
                                                  (setf contador (1+ contador)) (remove-de-lista-aux (rest lista-in)))))))
      (remove-de-lista-aux lista))
    (return-from remove-de-lista new-lista)))

;;avaliacao_f: estado x heuristica x custo-caminho -> inteiro
(defun avaliacao_f (estado heuristica custo-caminho)
  (+ (funcall heuristica estado) (funcall custo-caminho estado)))


;;melhor-estado: lista_de_estados x heuristica x custo-caminho -> inteiro
(defun melhor-estado (lista-de-estados heuristica custo-caminho)
  (let ((melhor-f (avaliacao_f (nth 0 (nth 0 lista-de-estados)) heuristica custo-caminho))
        (indice-melhor-estado 0))
    (loop for i from 1 to (1- (list-length lista-de-estados))
          do(when (<= (avaliacao_f (nth 0 (nth i lista-de-estados)) heuristica custo-caminho) melhor-f)
              (progn (setf melhor-f (avaliacao_f (nth 0 (nth i lista-de-estados)) heuristica custo-caminho))
                     (setf indice-melhor-estado i))))
    (return-from melhor-estado indice-melhor-estado)))

;;estado-em-lista: lista-de-estados x estado -> logico
(defun estado-em-lista (lista-de-estados estado)
  (loop for i from 0 to (list-length lista-de-estados)
        do(when (estados-iguais-p estado (nth i lista-de-estados)) (return-from estado-em-lista t)))
  (return-from estado-em-lista nil))

;;gera-estados: estado x accoes x resultado -> lista
(defun gera-estados (par-estado-accoes accoes resultado)
  (let ((lista-de-estados ())
        (lista-de-accoes (funcall accoes (nth 0 par-estado-accoes))))
    (loop for i from 0 to (1- (list-length lista-de-accoes))
          do (setf lista-de-estados (append lista-de-estados (make-list 1 :initial-element (list (funcall resultado (nth 0 par-estado-accoes) (nth i lista-de-accoes)) (append (nth 1 par-estado-accoes) (list (nth i lista-de-accoes))))))))
    (return-from gera-estados lista-de-estados)))

;;procura-A*: problema x heuristica -> lista
(defun procura-A* (problema heuristica)
  (let ((accoes               (problema-accoes problema))
        (resultado            (problema-resultado problema))
        (solucao              (problema-solucao problema))
        (custo-caminho        (problema-custo-caminho problema))
        (estados-por-avaliar  (make-list 1 :initial-element (list (problema-estado-inicial problema) ())))
        (indice-melhor-estado 0)
        (par-estado-accao     0))
    (loop while (not (eql estados-por-avaliar nil))
          do(progn
              (setf indice-melhor-estado (melhor-estado estados-por-avaliar heuristica custo-caminho))
              (setf par-estado-accao     (nth indice-melhor-estado estados-por-avaliar))
              (setf estados-por-avaliar  (remove-de-lista estados-por-avaliar indice-melhor-estado))
              (when (funcall solucao (nth 0 par-estado-accao)) (return-from procura-A* (nth 1 par-estado-accao)))
              (setf estados-por-avaliar (append estados-por-avaliar (gera-estados par-estado-accao accoes resultado)))))
    (return-from procura-A* nil)))

;;heuristica-de-pesos: estado -> inteiro
#|(defun heuristica-de-pesos (estado-a-testar)
  (let ((tabuleiro-resultante (copia-tabuleiro estado-tabuleiro estado-a-testar)) (a  -0.51006) (b 0.760666) (c -0.35663) (d -0.184483) (peso-agregado 0) (linhas-completas 0) (relevo 0) (buraco 0) (aux-buraco-bool 0))
    ;;peso-agregado/relevo/buracos
    (loop for i from 0 to 9 do
          (progn(+ peso-agregado (tabuleiro-altura-coluna tabuleiro-resultante i))
          (when (/= i 9)(+ relevo (abs (- (tabuleiro-altura-coluna tabuleiro-resultante i) (tabuleiro-altura-coluna tabuleiro-resultante (+ 1 i))))))
          ;;contar buracos
          (loop for j from 0 to (tabuleiro-altura-coluna tabuleiro-resultante i) do
            (progn (when (and (= aux-buraco-bool 0)(not (tabuleiro-preenchido-p tabuleiro-resultante (- (tabuleiro-altura-coluna tabuleiro-resultante i) j) i))) (+ 1 buraco))
            (when (and (= aux-buraco-bool 1) (tabuleiro-preenchido-p tabuleiro-resultante))(setf aux-buraco-bool 0))))))
    ;;linhas-completas
    (loop for i from 17 do
      (when (tabuleiro-linha-completa-p (estado-tabuleiro estado-a-testar) i) (+ 1 linhas-completas)))
    return-from heuristica-de-pesos (+ (* a peso-agregado) (* b linhas-completas) (* c buracos) (* d relevo))))

;;procura-best: array x lista pecas -> lista accoes
#|(defun procura-best (array-tabuleiro pecas-por-colocar)
  (let problema-novo (make-problema :estado inicial (make-estado :pontos 0))
  ))|#

;;(load (compile-file "utils.lisp"))
;;(load "utils.fas")
