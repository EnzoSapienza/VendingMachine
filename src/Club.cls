"Filed out from Dolphin Smalltalk"!

Object subclass: #Club
	instanceVariableNames: 'nombre direccion telefono email empleados usuarios reservas canchas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Club guid: (GUID fromString: '{ef930ddc-8c14-4387-a272-ea540e149956}')!

Club comment: ''!

!Club categoriesForClass!Kernel-Objects! !

!Club methodsFor!

agregarCancha:unCancha
canchas add: unCancha.!

agregarEmpleado:unEmpleado
empleados add: unEmpleado.!

agregarReserva:unReserva
reservas add: unReserva.!

agregarUsuario:unUsuario
usuarios add: unUsuario.!

buscarCancha:idCancha
^ canchas detect: [:cancha | cancha verId = idCancha ] ifNone: [nil].!

buscarEmpleado:idEmpleado
^empleados detect: [:empleado | empleado verId = idEmpleado ] ifNone: [nil].!

buscarReserva:idReserva
^reservas detect: [:reserva | reserva verId = idReserva ] ifNone: [nil].!

buscarUsuario:idUsuario
^usuarios detect: [:usuario | usuario verNroUsuario = idUsuario ] ifNone: [nil].!

cantidadCanchas
^canchas size.!

cantidadEmpleados
^empleados size.!

cantidadReservas
^reservas size.!

cantidadUsuarios
^usuarios size.!

initClubnom:unNom dir:unaDir tel:unTel email:unEmail
nombre:= unNom.
direccion:= unaDir.
telefono:= unTel.
email:= unEmail.
empleados:= OrderedCollection new.
usuarios:= OrderedCollection new.
reservas := OrderedCollection new.
canchas:= OrderedCollection new.
!

modiDir:unaDir
direccion:=unaDir.!

modiEmail:unEmail
email:=unEmail.!

modiNom:unNom
nombre:=unNom.!

modiTel:unTel
telefono:=unTel.!

reservasDia: unaFecha
|reservas|
reservas := self verTodasLasReservas.
^reservas select: [:reserva | reserva verFecha = unaFecha ].!

verDir
^direccion.!

verEmail
^email.!

verNom
^nombre.!

verReservasActivas: unaFecha
|r|
r := self verTodasLasReservas.
^r select: [:reserva | (reserva verFecha = unaFecha) and: [reserva verEstadoReserva] ].
!

verTel
^telefono.!

verTodasLasCanchas
^canchas.!

verTodasLasReservas
^reservas.!

verTodosLosEmpleados
^empleados.!

verTodosLosUsuarios
^usuarios.! !

!Club categoriesForMethods!
agregarCancha:!public! !
agregarEmpleado:!public! !
agregarReserva:!public! !
agregarUsuario:!public! !
buscarCancha:!public! !
buscarEmpleado:!public! !
buscarReserva:!public! !
buscarUsuario:!public! !
cantidadCanchas!public! !
cantidadEmpleados!public! !
cantidadReservas!public! !
cantidadUsuarios!public! !
initClubnom:dir:tel:email:!public! !
modiDir:!public! !
modiEmail:!public! !
modiNom:!public! !
modiTel:!public! !
reservasDia:!public! !
verDir!public! !
verEmail!public! !
verNom!public! !
verReservasActivas:!public! !
verTel!public! !
verTodasLasCanchas!public! !
verTodasLasReservas!public! !
verTodosLosEmpleados!public! !
verTodosLosUsuarios!public! !
!

!Club class methodsFor!

crearClubnom: unNom dir: unaDir tel: unTel email: unEmail
^(self new) initClubnom: unNom dir: unaDir tel: unTel email: unEmail.! !

!Club class categoriesForMethods!
crearClubnom:dir:tel:email:!public! !
!

