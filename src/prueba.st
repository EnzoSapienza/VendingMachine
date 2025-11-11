|hospital cliente1 maquina1 productosId prod1 prod2 prod3 maquina2 cafe1 respuesta1 respuesta2|
hospital:= Hospital nombre: 'Hospital Britanico'.

TablaCompras vaciarTabla.
TablaItems vaciarTabla.
TablaMedicos vaciarTabla.
TablaProductos vaciarTabla.

cliente1:= Medico nombre: 'Jorge'  saldo: 1000.
TablaMedicos add: cliente1.

maquina1:= MaquinaSnack ubicacion: 'Sala de espera' compras: OrderedCollection new snacks: OrderedCollection new.

maquina2:= MaquinaCafe ubicacion: 'Patio de comidas' compras: OrderedCollection new leche: 100 capsulas: 30 agua: 300 azucar: 200.

prod1:= Snack nombre: 'Papas fritas' precio: 1000 marca: 'Lays' peso: 30.
prod2:= Snack nombre: 'Gaseosa' precio: 700 marca: 'CocaCola' peso: 100.
prod3:= Snack nombre: 'Palitos' precio: 200 marca: 'Lays' peso: 50.

cafe1:= TipoCafe nombre: 'Cappuchino' precio: 1200 azucar: 100 leche: 20 agua: 50.

TablaProductos add: prod1; add: prod2; add: prod3; add: cafe1.

maquina1 addSnack: prod1 getId stock: 30.
maquina1 addSnack: prod2 getId stock: 10.
maquina1 addSnack: prod3 getId stock: 20.

hospital addMaquina: maquina1.
hospital addMaquina: maquina2.

respuesta1:= ''.
respuesta2:= ''.
productosId:= OrderedCollection new.
[respuesta2 = 'salir']
whileFalse: [
	[respuesta2 =  'confirmar']
	whileFalse: [
		respuesta1:= (Prompter prompt: 'Ingrese un ID de snack: ').
		(respuesta1 asNumber > 0)
			ifTrue:[productosId add: respuesta1 asNumber asInteger. Transcript show: respuesta1; cr].
		respuesta2:= (Prompter prompt: 'Desea confirmar su compra sin añadir mas productos? Escriba "confirmar": ').
	].
	(hospital comprar: (maquina1 getId) productos: productosId pago: #PagoCredito datoCliente: cliente1 getId) inspect.
	respuesta2:= (Prompter prompt: 'Desea salir y no seguir comprando? Escriba "salir": ').

].

(hospital comprar: (maquina1 getId) productos: {prod1 getId. prod2 getId. prod3 getId.} pago: #PagoCredito datoCliente: cliente1 getId).

(hospital comprar: (maquina1 getId) productos: {prod1 getId. prod2 getId.} pago: #PagoQR datoCliente: 'prueba').

(hospital comprar: (maquina2 getId) productos: {cafe1 getId.} pago: #PagoCredito datoCliente: cliente1 getId).

TablaCompras inspect.
TablaProductos inspect.
