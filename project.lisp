
;;2.1.1 Tipos de Accao

;;cria-accao: inteiro x array -> accao
(defun cria-accao (inteiro array)
 (cons inteiro array)) 

;;accao-coluna: accao -> inteiro
(defun accao-coluna(accao)
  (car accao))

;;accao-peca: accao -> array
(defun accao-peca(accao)
  (cdr accao))

;;2.1.2 Tipo tabuleiro

;;cria-tabuleiro: -> tabuleiro
(defun cria-tabuleiro ()
  (make-array '(18 10)))

;;copia-tabuleiro: tabuleiro -> tabuleiro
(defun copia-tabuleiro(tabuleiro)
    (let ((tabuleiroNovo (make-array '(18 10))))
          (loop for i from 0 to 17
                do(loop for j from 0 to 9
                        do(setf (aref tabuleiroNovo i j) (aref tabuleiro i j))))
    (return-from copia-tabuleiro tabuleiroNovo)))

;;tabuleiro-preenchido-p: tabuleiro x inteiro -> logico
(defun tabuleiro-preenchido-p (tabuleiro inteiroLinha inteiroColuna)
  (aref tabuleiro inteiroLinha inteiroColuna))

;;tabuleiro-altura-coluna: tabuleiro x inteiro -> inteiro
(defun tabuleiro-altura-coluna (tabuleiro inteiro)
  (loop for i from 0 to 17
        do(cond ((eql (aref tabuleiro (- 17 i) inteiro) t) (return-from tabuleiro-altura-coluna (- 18 i)))
          )
        )
  (return-from tabuleiro-altura-coluna 0)
  )

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
  (setf (aref tabuleiro inteiroLinha inteiroColuna) t)
  )

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
(defun tabuleiro->array(tabuleiro)
  (let ((array (make-array '(18 10))))
    (setf array  (copia-tabuleiro tabuleiro))
    (return-from tabuleiro->array array)))

;;array->tabuleiro: array -> tabuleiro
(defun array->tabuleiro(array)
  (let ((tabuleiro (cria-tabuleiro)))
    (loop for i from 0 to 17
          do(loop for j from 0 to 9
                  do(cond((eql (aref array i j) t) (tabuleiro-preenche! tabuleiro i j)))))
  (return-from array->tabuleiro tabuleiro)))

;;2.1.3 Tipo Estado

;;struct estado
(defstruct estado (pontos :0) pecas-por-colocar pecas-colocadas tabuleiro)

;;copia-estado: estado -> estado
(defun copia-estado (estado)
    (copy-estado estado))

;;estados-iguais-p: estado x estado -> logico
(defun estados-iguais-p (estado1 estado2)
  (cond ((not (eql (estado-pontos estado1) (estado-pontos estado2))) nil)
        ((not (tabuleiros-iguais-p (estado-tabuleiro estado1) (estado-tabuleiro estado2))) nil)
        ((not (equal (estado-pecas-por-colocar estado1) (estado-pecas-por-colocar estado2))) nil)
        ((not (equal (estado-pecas-colocadas estado1) (estado-pecas-colocadas estado2))) nil)
        (t t)))

;;estado-final-p: estado -> logico
(defun estado-final-p (estado)
  (cond ((tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) t )
        ((equal (estado-pecas-colocadas estado) nil) t)))

;;2.1.4 Tipo problema

;;struct problema
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

;;2.2 Funcoes a implementar
;;2.2.1 Funcoes do problema de procura

;;solucao: estado -> logico
(defun solucao(estado)
  (cond ((tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) nil)
        ((equal (estado-pecas-por-colocar estado) nil) t)))

;;accoes: estado -> lista de accoes
;;TODO

;;resultado: estado x accao -> estado
;;Comecamos a fazer esta funcao mas tivemos de parar
#|(defun resultado(estado accao)
  (let ((estadoNovo (copia-estado estado)))
    (setf (estado-pecas-colocadas estadoNovo) (append (list (first (estado-pecas-por-colocar estado))) (estado-pecas-colocadas estado)))
    (setf (estado-pecas-por-colocar estadoNovo) (rest (estado-pecas-por-colocar estado)))
    percorrer coluna ate encontrar 1º espaco vazio antes de um espaco ocupado para comecar a colocar a peca
    ))|#


;;qualidade: estado->inteiro
;;TODO

;;custo-oportunidade: estado -> inteiro
;;TODO

