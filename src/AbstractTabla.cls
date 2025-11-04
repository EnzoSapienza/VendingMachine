"Filed out from Dolphin Smalltalk"!

Object subclass: #AbstractTabla
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'Collection Id'!

AbstractTabla guid: (GUID fromString: '{3bf80c10-84e7-44d9-ac83-89174c1bb6a1}')!

AbstractTabla comment: ''!

!AbstractTabla categoriesForClass!Kernel-Objects! !

!AbstractTabla class methodsFor!

add:unElemento
self subclassResponsibility.!

establecerTabla
	"Private - Instanciar tabla si no existe"

	Collection = nil
		ifTrue: 
			[Collection := Dictionary new.
			Id := 1]!

getAll
	^Collection values.!

getPorId: unId
	self establecerTabla.
	^Collection at: unId!

new
	self error: 'Las clase ' , self class printString , ' es una clase abstracta.'! !

!AbstractTabla class categoriesForMethods!
add:!public! !
establecerTabla!private! !
getAll!public! !
getPorId:!public! !
new!public! !
!

