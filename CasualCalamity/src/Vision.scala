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

  def absolutePositionOfFarthest(c: Char): Option[XY] = {
    var farthestPosition : Option[XY] = None
    var farthestDistance = Double.MaxValue
    for(i <- 0 until cells.length) {
      if(c == cells(i)) {
        val pos = absPosFromIndex(i)
        val distanceToCenter = pos.distanceTo(center)
        if(distanceToCenter > farthestDistance) {
          farthestDistance = distanceToCenter
          farthestPosition = Some(pos)
        }
      }
    }
    farthestPosition
  }

  def absolutePositionOfNearest(c: Char): Option[XY] = {
    var nearestPosition : Option[XY] = None
    var nearestDistance = Double.MaxValue
    for(i <- 0 until cells.length) {
      if(c == cells(i)) {
        val pos = absPosFromIndex(i)
        val distanceToCenter = pos.distanceTo(center)
        if(distanceToCenter < nearestDistance) {
          nearestDistance = distanceToCenter
          nearestPosition = Some(pos)
        }
      }
    }
    nearestPosition
  }

  def offsetToNearest(c: Char): Option[XY] = {
    absolutePositionOfNearest(c) match {
      case Some(xy: XY) => Some (relPosFromAbsPos(xy))
      case None => None
    }
  }
}
