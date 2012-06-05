(defparameter *ht* (make-hash-table))

(defun recorre(directorio)
  (let((archivo 0))
		(dolist (file (directory (make-pathname :type "pdf" :name :wild :defaults directorio)))
			(setf (gethash archivo *ht*) file)
			(setf archivo (+ archivo 1))
		)
	)
)
(recorre "/home/addiaz/Escritorio/III progra/")

(defun itera(tabla)
(loop for k being the hash-keys in tabla using(hash-value value)
do (lectura(gethash k tabla)))
)


(defun lectura (dir) (let ((in (open dir :element-type '(unsigned-byte 8) )))

	(when in 
		(let ((str (make-string (file-length in))))
		(dotimes (i (file-length in))  
			(setf (char str i) (code-char (read-byte in))))
			
		;(buscar-titulo str)
		(buscar-autor str)
		;(buscar-creador str)
		;(buscar-fecha str)
		;(buscar-productor str)
		;(buscar-palabras-claves str)
))))


(defun buscar-etiqueta (etiqueta str)
	(search etiqueta str))
	
(defun buscar-titulo(str)
	(format t "Titulo: ~a~%"(subseq str( + (buscar-etiqueta "/Title" str) 7)			  
									   ( + (buscar-etiqueta ")" (subseq str(buscar-etiqueta "/Title" str)(-(length str) 1))) (buscar-etiqueta "/Title" str)))))

(defun buscar-autor(str)
	(format t "Autor: ~a~%"(subseq str( + (buscar-etiqueta "/Author" str) 8 )			  
									  ( + (buscar-etiqueta ")" (subseq str(buscar-etiqueta "/Author" str)(-(length str) 1))) (buscar-etiqueta "/Author" str)))))
	
(defun buscar-creador(str)
	(format t "Creador: ~a~%"(subseq str( + (buscar-etiqueta "/Creator" str) 11 )  
										( + (buscar-etiqueta ")" (subseq str(buscar-etiqueta "/Creator" str)(-(length str) 1))) (buscar-etiqueta "/Creator" str)))))

(defun buscar-fecha(str)
	(format t "Fecha de creacion: ~a~%"(subseq str( + (buscar-etiqueta "/CreationDate" str) 16 )
												  ( +  (buscar-etiqueta "/CreationDate" str) 24))))

(defun buscar-productor(str)
	(format t "Productor: ~a~%"(subseq str( + (buscar-etiqueta "/Producer" str) 12 )
										  ( + (buscar-etiqueta ")" (subseq str(buscar-etiqueta "/Producer" str)(-(length str) 1))) (buscar-etiqueta "/Producer" str)))))

(defun buscar-palabras-claves(str)
	(format t "Palabras claves: ~a~%"(subseq str( + (buscar-etiqueta "/Keywords" str) 12 )
										  ( + (buscar-etiqueta ")" (subseq str(buscar-etiqueta "/Keywords" str)(-(length str) 1))) (buscar-etiqueta "/Keywords" str)))))
