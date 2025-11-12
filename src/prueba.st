| hospital cliente1 maquina1 ListaDeProductos productosId prod1 prod2 prod3 maquina2 cafe1 cafe2 cafe3 respuesta maquinaAComprar tipoPago datoCliente |
"Reiniciar tablas"
TablaCompras vaciarTabla.
TablaItems vaciarTabla.
TablaMedicos vaciarTabla.
TablaProductos vaciarTabla.

"Crear hospital con maquinas y productos"
hospital := Hospital nombre: 'Hospital Britanico'.
cliente1 := Medico nombre: 'Jorge' saldo: 1000.
TablaMedicos add: cliente1.
maquina1 := MaquinaSnack
			ubicacion: 'Sala de espera'
			compras: OrderedCollection new
			snacks: OrderedCollection new.
maquina2 := MaquinaCafe
			ubicacion: 'Patio de comidas'
			compras: OrderedCollection new
			leche: 1000
			capsulas: 300
			agua: 3000
			azucar: 2000.
prod1 := Snack
			nombre: 'Papas fritas'
			precio: 1000
			marca: 'Lays'
			peso: 30.
prod2 := Snack
			nombre: 'Gaseosa'
			precio: 700
			marca: 'CocaCola'
			peso: 100.
prod3 := Snack
			nombre: 'Palitos'
			precio: 200
			marca: 'Lays'
			peso: 50.
cafe1 := TipoCafe
			nombre: 'Cappuchino'
			precio: 1200
			azucar: 100
			leche: 20
			agua: 50.
cafe2 := TipoCafe
			nombre: 'Cafe con leche'
			precio: 1000
			azucar: 60
			leche: 120
			agua: 40.
cafe3 := TipoCafe
			nombre: 'Cafe solo'
			precio: 700
			azucar: 20
			leche: 10
			agua: 60.
TablaProductos
	add: prod1;
	add: prod2;
	add: prod3;
	add: cafe1;
	add: cafe2;
	add: cafe3.
maquina1 addSnack: prod1 getId stock: 30.
maquina1 addSnack: prod2 getId stock: 10.
maquina1 addSnack: prod3 getId stock: 20.
hospital addMaquina: maquina1.
hospital addMaquina: maquina2.

"Menu"
respuesta := ''.
[respuesta = 'salir' | respuesta isNil] whileFalse: 
		["Elegir máquina"
		[respuesta = 'snacks' | (respuesta = 'cafe')]
			whileFalse: [respuesta := Prompter prompt: 'Elige qué comprar (snacks | cafe):'].
		respuesta = 'snacks' ifTrue: [maquinaAComprar := maquina1].
		respuesta = 'cafe' ifTrue: [maquinaAComprar := maquina2].

		"Instanciar variables del menú"
		productosId := OrderedCollection new.
		ListaDeProductos := TablaProductos getAll inject: ''
					into: 
						[:acum :producto |
						((respuesta = 'snacks' and: [producto isKindOf: Snack])
							or: [respuesta = 'cafe' and: [producto isKindOf: TipoCafe]])
								ifTrue: [acum , producto getNombre , '(' , producto getId printString , ') ']
								ifFalse: [acum]].

		"Elegir productos"
		[((respuesta = 'confirmar') | (respuesta isNil)) & (productosId size > 0)] whileFalse: 
				[respuesta := Prompter
							prompt: 'Ingrese un ID de snack: ' , ListaDeProductos , '"confirmar" para seguir'.
				((respuesta size > 0 and: [respuesta allSatisfy: [:d | d isDigit]]) and: [respuesta asNumber > 0])
					ifTrue: [productosId add: respuesta asNumber asInteger]].

		"Elegir tipo de pago"
		[respuesta = 'tarjeta' | (respuesta = 'qr') | (respuesta = 'credito')]
			whileFalse: [respuesta := Prompter prompt: 'Elige qué comprar (tarjeta | qr | credito):'].
		respuesta = 'tarjeta'
			ifTrue: 
				[tipoPago := #PagoTarjeta.
				datoCliente := (Prompter prompt: 'Ingrese número de tarjeta:') asNumber asInteger].
		respuesta = 'qr'
			ifTrue: 
				[tipoPago := #PagoQR.
				datoCliente := Prompter prompt: 'Ingrese link de cuenta:'].
		respuesta = 'credito'
			ifTrue: 
				[tipoPago := #PagoCredito.
				datoCliente := (Prompter prompt: 'Ingrese número de cuenta (nota: use la cuenta 1):') asNumber
							asInteger].

		"Realizar compra"
		 MessageBox notify: (hospital
			comprar: maquinaAComprar getId
			productos: productosId
			pago: tipoPago
			datoCliente: datoCliente) getMensaje.
		respuesta := Prompter prompt: 'Desea salir y no seguir comprando? Escriba "salir": '].

"Compras de ejemplo"
hospital
	comprar: maquina1 getId
	productos: {prod1 getId. prod2 getId. prod3 getId}
	pago: #PagoCredito
	datoCliente: cliente1 getId.
hospital
	comprar: maquina1 getId
	productos: {prod1 getId. prod2 getId}
	pago: #PagoQR
	datoCliente: 'ivan.kc.mercadopago'.
hospital
	comprar: maquina2 getId
	productos: {cafe1 getId}
	pago: #PagoCredito
	datoCliente: cliente1 getId