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
(defun tabuleiro-preenchido-p (tabuleiro inteiro inteiro)
  (aref tabuleiro inteiro inteiro inteiro))

;;tabuleiro-altura-coluna: tabuleiro x inteiro -> inteiro
(defun tabuleiro-altura-coluna (tabuleiro inteiro)
  (loop for i from 17 to 0
        do(if ((aref tabuleiro i inteiro) t)
              (return-from tabuleiro-altura-coluna i)))
        (return-from tabuleiro-altura-coluna nil))
