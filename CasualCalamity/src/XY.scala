case class XY(x: Int, y: Int) {
  override def toString = x + ":" + y

  def isNonZero = x!=0 || y!=0
  def isZero = x==0 && y==0
  def isNonNegative = x>=0 && y>=0

  def +(pos: XY) = XY(x+pos.x, y+pos.y)
  def -(pos: XY) = XY(x-pos.x, y-pos.y)
  def *(factor: Double) = XY((x*factor).intValue, (y*factor).intValue)

  def distanceTo(pos: XY) : Double = (this-pos).length
  def length : Double = math.sqrt(x*x + y*y)

  def signum = XY(x.signum, y.signum)

  def negate = XY(-x, -y)
  def negateX = XY(-x, y)
  def negateY = XY(x, -y)

  def updateX(newX: Int) = XY(newX, y)
  def updateY(newY: Int) = XY(x, newY)
}

object XY {
  val ZERO = XY(0, 0)
  val ONE  = XY(1, 1)

  val Right      = XY( 1,  0)
  val RightUp    = XY( 1, -1)
  val Up         = XY( 0, -1)
  val UpLeft     = XY(-1, -1)
  val Left       = XY(-1,  0)
  val LeftDown   = XY(-1,  1)
  val Down       = XY( 0,  1)
  val DownRight  = XY( 1,  1)

  def apply(s: String): XY = {
    val xy = s.split(':').map(_.toInt) // e.g. "-1:1" => Array(-1,1)
    XY(xy(0), xy(1))
  }
}