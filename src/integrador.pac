| package |
package := Package name: 'integrador'.
package paxVersion: 1;
	basicComment: ''.

package classNames
	add: #Hospital;
	add: #MaquinaExpendedora;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\..\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin').

package!

"Class Definitions"!

Object subclass: #Hospital
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #MaquinaExpendedora
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"End of package definition"!

"Source Globals"!

"Classes"!

Hospital guid: (GUID fromString: '{8d2fe85d-8d61-4e20-ac72-c87540dcc737}')!

Hospital comment: ''!

!Hospital categoriesForClass!Kernel-Objects! !

!Hospital methodsFor!

verMaquinasExp
	^0.! !

!Hospital categoriesForMethods!
verMaquinasExp!public! !
!

MaquinaExpendedora guid: (GUID fromString: '{a06523da-c26c-46c1-a1b7-fea446bf6b2d}')!

MaquinaExpendedora comment: ''!

!MaquinaExpendedora categoriesForClass!Kernel-Objects! !

!MaquinaExpendedora methodsFor!

new
	self = MaquinaExpendedora ifTrue: [ 
		self error: 'MaquinaExpendedora es abstracta y no puede instanciarse directamente' ].
	^super new.! !

!MaquinaExpendedora categoriesForMethods!
new!public! !
!

"Binary Globals"!

