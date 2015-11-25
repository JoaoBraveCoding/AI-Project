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
                 do(when (and (aref peca i j) (aref tabuleiro (1- (+ i altura)) (+ j coluna))) (return-from descer nil))))
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
          (setf linha (1- linha)))

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


;;melhor-estado: lista_de_estados x avaliacao_f -> inteiro
(defun melhor-estado (lista-de-estados avaliacao_f)
  (let ((melhor-f (funcall avaliacao_f (nth 0 lista_de_estados))) (indice-melhor-estado 0))
  (loop for i from 1 to (- 1 (list-length lista-de-estados))
    do(when (<= (funcall avaliacao_f (nth i lista-de-estados))(melhor-f))(progn (setf melhor-f (funcall avaliacao-f (nth i lista-de-estados)))(setf indice-melhor-estado i))
  ))
  (return-from melhor-estado indice-melhor-estado)))

;;estado-em-lista: lista-de-estados x estado -> logico
(defun estado-em-lista (lista-de-estados estado)
  (loop for i from 0 to (list-length lista-de-estados)
    do(when ((estados-iguais-p estado (nth i lista-de-estados)))(return t)))
  return nil)

;;(load (compile-file "utils.lisp"))
;;(load "utils.fas")
