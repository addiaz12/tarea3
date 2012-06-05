;III proyecto programado, Lenguajes de programacion
;Adrian Diaz Meza, John Largaespada perez, Alonso Vargas Astua

;--------------------------------------------------------------------------------------------------------------------------------------
;se define una base de datos donde estaran almacenados todos los datos de los pdf
(defun bd_pdf(titulo autor asunto palabras_clave fecha_creacion )
(list :titulo titulo  :autor autor :asunto asunto :palabras_clave palabras_clave :fecha_creacion fecha_creacion))
(defvar *db* nil)
;La funcion agrega introduce un dato nuevo a la base de datos
(defun agrega(bd_pdf) (push bd_pdf *db*))
;la tabla ficheros almacema temporalmente las direcciones de los ficheros pdf
(defparameter *tabla_ficheros* (make-hash-table))

;--------------------------------------------------------------------------------------------------------------------------------------
;las funciones busca_label, titulo, autor, asunto, fecha_creacion, palabras_clave 
;se encargan de buscar los metadatos dentro de un fichero
(defun busca_label (label str)
	(search label str))
	
(defun titulo(str)
	(subseq str( + (busca_label "/Title" str) 7)			  
		( + (busca_label ")" (subseq str(busca_label "/Title" str)(-(length str) 1))) (busca_label "/Title" str))))

(defun autor(str)
	(subseq str( + (busca_label "/Author" str) 8 )			  
		( + (busca_label ")" (subseq str(busca_label "/Author" str)(-(length str) 1))) (busca_label "/Author" str))))
	
(defun asunto(str)
	(subseq str( + (busca_label "/Subject" str) 11 )  
		( + (busca_label ")" (subseq str(busca_label "/Subject" str)(-(length str) 1))) (busca_label "/Subject" str))))

(defun fecha_creacion(str)
	(subseq str( + (busca_label "/CreationDate" str) 16 )
		( +  (busca_label "/CreationDate" str) 24)))

(defun palabras_claves(str)
	(subseq str( + (busca_label "/Keywords" str) 12 )
		( + (busca_label ")" (subseq str(busca_label "/Keywords" str)(-(length str) 1))) (busca_label "/Keywords" str))))
		
;--------------------------------------------------------------------------------------------------------------------------------------
;la funcion extraer datos toma los metadatos que se obtienen con las funciones de busqueda y los almacenan en la base de datos
(defun extaer_metadatos (directorio) (let ((in (open directorio :element-type '(unsigned-byte 8) )))
	(when in 
		(let ((str (make-string (file-length in))))
			(dotimes (i (file-length in))  
				(setf (char str i) (code-char (read-byte in))))
			(agrega(bd_pdf (titulo str) (autor str) (asunto str) (palabras_claves str) (fecha_creacion str)))))))
			
;--------------------------------------------------------------------------------------------------------------------------------------
;las funciones select_titulo, select_autor, select_asunto, select_palabras_claves y select_fecha_creacion 
;realizan consultas a la base de datos con un parametro y retornan las filas con las que hay coincidencias
(defun select_titulo(titulo)
(remove-if-not #'(lambda (bd_pdf) (equal (getf bd_pdf :titulo) titulo)) *db*))
   
(defun select_autor(autor)
  (remove-if-not  #'(lambda (bd_pdf) (equal (getf bd_pdf :autor) autor))*db*))
   
(defun select_asunto(asunto)
  (remove-if-not #'(lambda (bd_pdf) (equal (getf bd_pdf :asunto) asunto)) *db*))
   
 (defun select_palabras_clave(palabras_clave)
  (remove-if-not  #'(lambda (bd_pdf) (equal (getf bd_pdf :palabras_clave) palabras_clave))*db*))
   
(defun select_fecha_creacion(fecha_creacion)
  (remove-if-not #'(lambda (bd_pdf) (equal (getf bd_pdf :fecha_creacion) fecha_creacion)) *db*))
   
;--------------------------------------------------------------------------------------------------------------------------------------
;la funcion itera recorre el contenido de la tabla hash que contiene los directorios y 
;los envia a las funciones que extren los metadatos
(defun itera(tabla)
	(loop for k being the hash-keys in tabla using(hash-value value)
		do (extaer_metadatos(gethash k tabla))))

;la funcion recorre extrae todos los archivos de tipo pdf que exinten en un directorio y los almacena en una tabla hash
(defun recorre(directorio)
	(let((archivo 0))
		(dolist (file (directory (make-pathname :type "pdf" :name :wild :defaults directorio)))
			(setf (gethash archivo *tabla_ficheros*) file)
			(setf archivo (+ archivo 1))))
	(itera *tabla_ficheros*))
	
;--------------------------------------------------------------------------------------------------------------------------------------

;----------------------------------------------------------------------------------------------------------------------------
;funcion para imprimir forma de tabla
(defun imprime()
(format t "~&Nombre~20TTitulo~35TAutor~45TPalabras_Clave~65TFecha_creacion" )
(dolist (bd_pdf  *db* )
    (format t "~{~&~A~20T~A~35t~A~45T~A~65T~A~}" bd_ pdf *db* )))
;----------------------------------------------------------------------------------
;la funcion inicio recibe un directorio y lo manda a las funciones que extraen
;los nombres de los ficheros y metadatos para crear la base de datos
(defun inicio()
	(format t "~%ESCRIBA LA DIRECCION DEL DIRECTORIO ~% ")
	(setf dir (read))
	(recorre dir)
	(menu))

;la funcion menu muestra las opciones de consulta que el usuario puede realizar
(defun menu()
	(format t "~%CONSULTAS~% 1 = Titulo~% 2 = Autor~% 3 = Asunto~% 4 = Palabra Clave~% 5 = fecha~% 6 = todo~% 7 = volver~%")
	(setf opcion (read))
	(format t "~%Palabra a consultar~%")
	(setf palabra (read))
	(if (= opcion 1)
		(select_titulo palabra)
		(if(= opcion 2)
			(select_autor palabra)
			(if(= opcion 3)
				(select_asunto palabra)
				(if(= opcion 4)
					(select_palabras_clave palabra)
					(if(= opcion 5)
						(select_fecha_creacion palabra)
						(if(= opcion 6 )
							*db*
							(if(= opcion 7)
								(inicio)))))))))
;-------------------------------------------------------------------------------------------------------------------------------------
