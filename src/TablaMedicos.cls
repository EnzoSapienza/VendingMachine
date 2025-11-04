"Filed out from Dolphin Smalltalk"!

AbstractTabla subclass: #TablaMedicos
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

TablaMedicos guid: (GUID fromString: '{9492eed8-3e56-45d6-b9f2-875a1020791e}')!

TablaMedicos comment: ''!

!TablaMedicos categoriesForClass!Kernel-Objects! !

!TablaMedicos class methodsFor!

addMedico: medico
	(medico isKindOf: Medico) ifFalse: [self error: 'No es un medico'].
	self establecerTabla.
	medico setId: Id.
	Collection add: medico.
	Id := Id + 1! !

!TablaMedicos class categoriesForMethods!
addMedico:!public! !
!

