
;;;;;;;;;;;;;;;;;;;;;;	Template del paciente
(deftemplate persona
	(slot tarjeta_sanitaria (type INTEGER))
	(multislot name (type STRING))
	(slot DNI)
	(slot edad (type INTEGER) (range 0 100))
	(slot genero (type SYMBOL) (allowed-symbols m f))

	;;;;;;;;;;;;;;;;;Preguntas iniciales
	(slot fiebre (type SYMBOL) (allowed-symbols n s))
	(slot cansancio (type SYMBOL) (allowed-symbols n s))
	(slot tos (type SYMBOL) (allowed-symbols n s))
	(slot perdida-gusto/olfato (type SYMBOL) (allowed-symbols n s))
	(slot contacto+ (type SYMBOL) (allowed-symbols n s))

	;;;;;;;;;;;;;;;;;Preguntas especializadas
	;;;;;;;;;;;;;;Covid
	(slot test_antigenos (type SYMBOL) (allowed-symbols n s))
	(slot covid (type SYMBOL) (allowed-symbols n s))  
	;;;;;;;;;;;;;;Gripe
	(slot gripe (type SYMBOL) (allowed-symbols n s))  
)

(deffacts arranque (persona (tarjeta_sanitaria 1)(name "Celina")(DNI 24395393L)(edad 24)(genero f))
		   (persona (tarjeta_sanitaria 2)(name "Maria" )(DNI 62589290G)(edad 30)(genero f))
		   (persona (tarjeta_sanitaria 3)(name "David")(DNI 28402904H)(edad 60)(genero m))
		   (inicio)
)



;;;;;;;;;;;;;;;--------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;	                   INICIO
;;;;;;;;;;;;;;;--------------------------------------------------------------------------

(defrule ini
	(inicio)
	=>
	(printout t crlf "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" crlf)
	(printout t "                                 Consulta médica	      	                      		" crlf)
	(printout t "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" crlf crlf crlf crlf)
	(printout t "¿Tiene tarjeta sanitaria?" crlf)
	(printout t "Respuesta: ")
	(bind ?ts (lowcase (read)))
	;;;;;;;;;;;;;;;;; lowcase y el if else permite escribir si, SI, s, S, No, NO, n, N 
	(if (eq ?ts si) then
		(bind ?ts s)
		(assert (ts ?ts))
		(printout t "Introduza el número de tarjeta sanitaria:" crlf)
		(printout t "Respuesta: ")
		(assert(tarjeta_sanitaria(read)))

	else
		(if (eq ?ts no) then
			(bind ?ts n)
			(printout t "A continuación realizaremos el registro para la obtención de la tarjeta sanitaria..." crlf)
		)
	(assert (ts ?ts))

	)
)

(defrule actual
	(ts s)
	?tarjeta <-(tarjeta_sanitaria ?tarjeta_sanitaria)
	(tarjeta_sanitaria ?tarjeta_sanitaria)
	?persona <- (persona(tarjeta_sanitaria ?tarjeta_sanitaria))
	=>
	(modify ?persona (tarjeta_sanitaria ?tarjeta_sanitaria))
	(retract ?tarjeta)
)


;;;;;;;;;;;;;;;--------------------------------------------------------------------------
;;;;;;;;;;;;;;;                       Registro tarjeta sanitaria
;;;;;;;;;;;;;;;--------------------------------------------------------------------------

(defrule registro
	(ts n)
	=>
	(printout t crlf "Nombre del paciente:" crlf)
	(printout t "Respuesta: ")
	(bind ?name (readline))

	(printout t crlf "DNI de "?name":" crlf)
	(printout t "Respuesta: ")
	(bind ?dni (read))

	(printout t crlf "Edad de "?name":" crlf)
	(printout t "Respuesta: ")
	(bind ?edad (read))

	(printout t crlf "¿Cúal es el genero de "?name"? (maculino/femenino)" crlf)
	(printout t "Respuesta: ")
	(bind ?genero (lowcase (read)))
	(if (eq ?genero masculino) then
		(bind ?genero m)
	else
		(if (eq ?genero femenino) then
			(bind ?genero f)
		)
	)

	(printout t crlf "Introducir nuevo numero de tarjeta sanitaria:" crlf)
	(printout t "Respuesta: ")
	(bind ?tarjeta_sanitaria(read))

	(assert (persona (tarjeta_sanitaria ?tarjeta_sanitaria) (name ?name) (DNI ?dni) (edad ?edad) (genero ?genero)))

)


;;;;;;;;;;;;;;;--------------------------------------------------------------------------
;;;;;;;;;;;;;;;                 	Preguntas Generales:
;;;;;;;;;;;;;;;--------------------------------------------------------------------------


(defrule fiebre
	(not (fiebre ?))
	?persona <- (persona(tarjeta_sanitaria ?tarjeta_sanitaria))
	=>
	(printout t crlf "Tiene fiebre? (si/no)" crlf)
	(printout t "Respuesta: ")
	(bind ?fiebre (lowcase (read)))
	;;;;;;;;;;;;;;;;; lowcase y el if else permite escribir si, SI, s, S, No, NO, n, N 
	(if (eq ?fiebre si) then
		(bind ?fiebre s)
	else
		(if (eq ?fiebre no) then
			(bind ?fiebre n)
		)
	)
	(assert (fiebre ?fiebre))
	(modify ?persona (fiebre ?fiebre))
)

(defrule tos
	(not (tos ?))
	?persona <- (persona(tarjeta_sanitaria ?tarjeta_sanitaria))
	=>
	(printout t crlf "Tiene tos? (si/no)" crlf)
	(printout t "Respuesta: ")
	(bind ?tos (lowcase (read)))
	;;;;;;;;;;;;;;;;; lowcase y el if else permite escribir si, SI, s, S, No, NO, n, N 
	(if (eq ?tos si) then
		(bind ?tos s)
	else
		(if (eq ?tos no) then
			(bind ?tos n)
		)
	)
	(assert (tos ?tos))
	(modify ?persona (tos ?tos))
)

(defrule cansancio
	(not (cansancio ?))
	?persona <- (persona(tarjeta_sanitaria ?tarjeta_sanitaria))
	=>
	(printout t crlf "Tiene cansancio? (si/no)" crlf)
	(printout t "Respuesta: ")
	(bind ?cansancio (lowcase (read)))
	;;;;;;;;;;;;;;;;; lowcase y el if else permite escribir si, SI, s, S, No, NO, n, N 
	(if (eq ?cansancio si) then
		(bind ?cansancio s)
	else
		(if (eq ?cansancio no) then
			(bind ?cansancio n)
		)
	)
	(assert (cansancio ?cansancio))
	(modify ?persona (cansancio ?cansancio))
)


(defrule contacto+
	(not (contacto+ ?))
	?persona <- (persona(tarjeta_sanitaria ?tarjeta_sanitaria))
	=>
	(printout t crlf "Ha estado en contacto con una persona positiva en covid recientemente? (si/no)" crlf)
	(printout t "Respuesta: ")
	(bind ?contacto+ (lowcase (read)))
	;;;;;;;;;;;;;;;;; lowcase y el if else permite escribir si, SI, s, S, No, NO, n, N 
	(if (eq ?contacto+ si) then
		(bind ?contacto+ s)
	else
		(if (eq ?contacto+ no) then
			(bind ?contacto+ n)
		)
	)
	(assert (contacto+ ?contacto+))
	(modify ?persona (contacto+ ?contacto+))
)



;;;;;;;;;;;;;;;--------------------------------------------------------------------------
;;;;;;;;;;;;;;;                     Resultado: Covid o Gripe?
;;;;;;;;;;;;;;;--------------------------------------------------------------------------

(defrule covid
	(not (fin ?))
	(not (test_antigenos ?))
	?persona <- (persona(tarjeta_sanitaria ?tarjeta_sanitaria))
	(and (persona (covid n))  (or (persona (fiebre s)) (persona (tos s)) (persona (cansancio s)) (persona (contacto+ s)) ))
	=>
	(printout t crlf "A continuación se realizará un test de antigenos..." crlf)
	(printout t crlf "Resultado test covid: (si/no)" crlf)
	(printout t "Respuesta: ")
	(bind ?test_antigenos (lowcase (read)))
	(if (eq ?test_antigenos si) then
		(bind ?covid s)
		(printout t crlf "El resultado del test de antigenos es positivo, debe realizar cuarentena." crlf)
	else
		(if (eq ?test_antigenos no) then
			(bind ?covid n)
			(bind ?test_antigenos n)
		)
	)
	(modify ?persona (covid ?covid))
	(assert (test_antigenos ?test_antigenos))
	(assert (fin))
)


(defrule gripe
	(not (fin ?))
	(test_antigenos n)
	?persona <- (persona(tarjeta_sanitaria ?tarjeta_sanitaria))
	(and (persona (covid n))  (or (persona (fiebre s)) (persona (tos s)) (persona (cansancio s)) (persona (contacto+ s)) ))
	=>
	(printout t crlf "Es probable que sea gripe, haga reposo!" crlf)
	(modify ?persona (gripe s))
	(assert (fin))
)

;;;;;;;;;;;;;;;--------------------------------------------------------------------------
;;;;;;;;;;;;;;;                             Continuación del programa
;;;;;;;;;;;;;;;--------------------------------------------------------------------------

(defrule continuar
	(fin)
	?f1 <- (fin)
	?f2 <- (test_antigenos ?test_antigenos)
	?f3 <- (fiebre ?fiebre)
	?f4 <- (ts ?ts)
	?f5 <- (tos ?tos)
	?f6 <- (cansancio ?cansancio)
	?f7 <- (contacto+ ?contacto+)
	?f8 <- (inicio)
	=>
	(printout t crlf "Hay más pacientes?" crlf)
	(printout t "Respuesta: ")
	(bind ?continue (lowcase (read)))
	(if (eq ?continue si) then
		(retract ?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7 ?f8)
		(assert(inicio))
)
)
