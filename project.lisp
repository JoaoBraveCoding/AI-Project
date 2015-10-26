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
        do(cond ((eq (aref tabuleiro (- 17 i) inteiro) t) (return-from tabuleiro-altura-coluna (- 18 i)))
          )
        )
  (return-from tabuleiro-altura-coluna 0)
  )

;;tabuleiro-linha-completa-p: tabuleiro x inteiro -> logico
(defun tabuleiro-linha-completa-p (tabuleiro inteiro)
  (loop for i from 0 to 9
        do(cond ((eq (aref tabuleiro inteiro i) nil) (return-from tabuleiro-linha-completa-p nil))
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
        do(cond ((eq (aref tabuleiro 17 i) t) (return-from tabuleiro-topo-preenchido-p t))
                )
        )
  (return-from tabuleiro-topo-preenchido-p nil)
  )

;;tabuleiros-iguais-p: tabuleiro x tabuleiro -> logico
(defun tabuleiros-iguais-p (tabuleiro1 tabuleiro2)
  (loop for i from 0 to 9
        do(loop for j from 0 to 17
                do(cond((not(eq (aref tabuleiro1 j i) (aref tabuleiro2 j i))) (return-from tabuleiros-iguais-p nil))
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
                  do(cond((eq (aref array i j) t) (tabuleiro-preenche! tabuleiro i j)))))
  (return-from array->tabuleiro tabuleiro)))

;;2.1.3 Tipo Estado

;;struct estado
(defstruct estado (pontos :0) pecas-por-colocar pecas-colocadas tabuleiro)

;;copia-estado: estado -> estado
#|(defun copia-estado (estado)
  (let ((estadoNovo (make-estado))
        (list-pecas-por-colocar (make-list (lenght estado-pecas-por-colocar)))
        (list-pecas-colocadas (make-list (lenght estado-pecas-colocadas))))
    (setf estadoNovo-pontos estado-pontos)
    (loop for i from 0 to (length estado-pecas-por-colocar)
          do(setf (nth i list-pecas-por-colocar)  (nth i estado-pecas-por-colocar)))
    (setf estadoNovo-pecas-por-colocar list-pecas-por-colocar)
    (loop for i from 0 to (length estado-pecas-colocadas)
          do(setf (nth i list-pecas-colocadas)  (nth i estado-pecas-colocadas)))
    (setf estadoNovo-pecas-colocadas list-pecas-colocadas)
(setf estadoNovo-tabueiro (copia-tabuleiro estado-tabuleiro))))|#

