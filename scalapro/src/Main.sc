//Parcial Andres Carrasquilla

trait Tipo {
  type T
  def value: T
}

object Cheques extends Tipo {
  type T = String
  def value: T = "Cheques"
}

object Ahorros extends Tipo {
  type T = String
  def value: T = "Ahorros"
}

object Fiduciaria extends Tipo {
  type T = String
  def value: T = "Fiduciaria"
}



trait Estado {
  type Est
  def value: Est
}

object Nueva extends Estado {
  type Est = String
  def value: Est = "Nueva"
}

object Activa extends Estado {
  type Est = String
  def value: Est = "Activa"
}

object Cerrada extends Estado {
  type Est = String
  def value: Est = "Cerrada"
}

case class Cuenta (numeroCuenta: Int,
                   codigoPropietario: Int,
                   valorCuenta: Long,
                   tipoCuenta: Tipo,
                   estado: Estado,
                   estadoTransaccion: Boolean)

case class Transferencia(cuentaADebitar: Cuenta,
                         cuentaAConsignar: Cuenta,
                         estadoTransferencia: Boolean)

def debitar(cuenta: Cuenta, valorDebitar: Int): Cuenta ={
  if(cuenta.valorCuenta >= valorDebitar &&
    cuenta.estado == Activa){
    new Cuenta(cuenta.numeroCuenta ,
      cuenta.codigoPropietario,
      cuenta.valorCuenta - valorDebitar,
      cuenta.tipoCuenta,
      cuenta.estado, true)
  }
  else{
    new Cuenta(cuenta.numeroCuenta ,
      cuenta.codigoPropietario,
      cuenta.valorCuenta,
      cuenta.tipoCuenta,
      cuenta.estado, false)
  }

}

def consignar(cuenta: Cuenta, valorCosignacion: Int): Cuenta ={
  if(cuenta.estado == Cerrada){
    new Cuenta(cuenta.numeroCuenta,
      cuenta.codigoPropietario,
      cuenta.valorCuenta + valorCosignacion,
      cuenta.tipoCuenta, cuenta.estado, false)
  }
  else {
    new Cuenta(cuenta.numeroCuenta,
      cuenta.codigoPropietario,
      cuenta.valorCuenta + valorCosignacion,
      cuenta.tipoCuenta, Activa, true)
  }
}

def transferir(cuentaADebitar: Cuenta,
               cuentaAConsignar: Cuenta,
               valorTransferencia: Int): Transferencia ={
  val debito = debitar(cuentaADebitar, valorTransferencia)
  if(debito.estadoTransaccion &&
    cuentaADebitar.estado == Activa &&
    cuentaAConsignar.estado == Activa
  ) {
    val consign = consignar(cuentaAConsignar, valorTransferencia)
    new Transferencia(debito, consign, true)
  } else{
    new Transferencia(cuentaADebitar, cuentaAConsignar, false)
  }
}

def obtenerBalance(cuenta: Cuenta): Long ={
  cuenta.valorCuenta
}

def crearCuenta(numeroCuenta: Int,
                idPropietario: Int,
                tipoCuenta: Tipo): Cuenta ={
    new Cuenta (numeroCuenta,
      idPropietario,
      0,
      tipoCuenta,
      Nueva,
      false)
}

def cerrarCuenta(cuenta: Cuenta): Cuenta ={
  new Cuenta(cuenta.numeroCuenta ,
    cuenta.codigoPropietario,
    cuenta.valorCuenta,
    cuenta.tipoCuenta,
    Cerrada, false)
}

//Para probar
def validarTrue(actual: Any, esperado: Any): String={
  if(actual != esperado)
    "ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR"+
      "ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR"
  else
    "OK"
}

//PruebaCompleta
var ca1 = crearCuenta(1,10384,Ahorros)
validarTrue(ca1.estado, Nueva)
ca1 = debitar(ca1,200)
validarTrue(ca1.estadoTransaccion, false)
ca1 = consignar(ca1,100)
validarTrue(ca1.estadoTransaccion, true)
validarTrue(ca1.valorCuenta, 100)
validarTrue(ca1.estado, Activa)
ca1 = debitar(ca1,900)
validarTrue(ca1.estadoTransaccion, false)
ca1 = consignar(ca1,300)
validarTrue(ca1.valorCuenta, 400)


var cb = crearCuenta(1,1036,Cheques)

//Transferencia incorrecta
val t1 = transferir(ca1,cb,100)
validarTrue(t1.estadoTransferencia, false)
ca1 = t1.cuentaADebitar
cb = t1.cuentaAConsignar

cb = consignar(cb,50)


//Ahora si se puede transferir
val t2 = transferir(ca1,cb,100)
validarTrue(t2.estadoTransferencia, true)
ca1 = t2.cuentaADebitar
cb = t2.cuentaAConsignar

ca1 = debitar(ca1, 300)
ca1 = cerrarCuenta(ca1)
validarTrue(ca1.estado, Cerrada)



