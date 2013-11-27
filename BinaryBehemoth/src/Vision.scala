class Vision(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size/2, size/2)

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
  def absPosFromIndex(index: Int) = XY(index % size, index / size)
  def absPosFromRelPos(relPos: XY) = relPos + center
  def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

  def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
  def relPosFromAbsPos(absPos: XY) = absPos - center
  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
  def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

  def offsetToNearest(c: Char): Option[XY] = {
    var nearestPosOpt : Option[XY] = None
    var nearestDistance = Double.MaxValue
    for(i <- 0 until cells.length) {
      if(c == cells(i)) {
        val pos = absPosFromIndex(i)
        val distanceToCenter = pos.distanceTo(center)
        if(distanceToCenter < nearestDistance) {
          nearestDistance = distanceToCenter
          nearestPosOpt = Some(pos - center)
        }
      }
    }
    nearestPosOpt
  }

}
