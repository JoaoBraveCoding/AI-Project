;;2.1.1 Tipos de Accao

;;struct accao
(defstruct accao inteiro array)

;;cria-accao: inteiro x array -> accao
(defun cria-accao (inteiro array)
  (make-accao :inteiro inteiro :array array))

;;accao-coluna: accao -> inteiro
(defun accao-coluna(accao)
  (accao-inteiro accao))

;;accao-peca: accao -> array
(defun accao-peca(accao)
  (accao-array accao))
