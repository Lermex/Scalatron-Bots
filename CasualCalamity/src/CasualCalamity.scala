import scala.util.Random

class CasualCalamity {
  var rnd = new Random()

  def respond(input: String) = {
    def splitParam(param: String) = {
      val segments = param.split('=')
      (segments(0), segments(1))
    }

    val tokens = input.split('(')
    val opcode = tokens(0)
    val params = tokens(1).dropRight(1).split(',').map(splitParam).toMap

    opcode match {
      case "Welcome" => welcome(params)
      case "React"   => react(params)
      case "Goodbye" => goodbye(params)
    }
  }

  def welcome(params: Map[String,String]) = {
    print("got welcome")
    ""
  }

  def goodbye(params: Map[String,String]) = {
    print("got goodbye")
    ""
  }

  def react(params: Map[String, String]) = params("generation").toInt match {
    case 0 => masterReact(params)
    case 1 => if (params("role") == "missile") missileReact(params) else mineReact(params)
  }

  def masterReact(params: Map[String, String]) = {
    moveSomewhatIntelligently(params("view"), params)
  }

  def missileReact(params: Map[String, String]) = {
    val vision = new Vision(params("view"))
    val offsetToMaster = vision.offsetToNearest('m')
    val offsetToSlave = vision.offsetToNearest('s')

    var nearestEnemy: Option[XY] = None

//    if (offsetToMaster.isDefined && offsetToSlave.isDefined) {
//      nearestEnemy = if (offsetToMaster.get.length > offsetToSlave.get.length) offsetToSlave else offsetToMaster
//    } else if (offsetToMaster.isDefined) {
//      nearestEnemy = offsetToMaster
//    } else if (offsetToSlave.isDefined) {
//      nearestEnemy = offsetToSlave
//    }

    nearestEnemy = offsetToMaster

    if (nearestEnemy.isDefined) {
      if (nearestEnemy.get.length <= 2) {
        "Status(text=missileBOOM)|Explode(size=2)"
      } else {
//          var direction: XY = null
//          val path: Option[List[Node]] = AStar(vision.center, nearestEnemy.get, vision)
//          if (path.isDefined) {
//            direction = vision.relPosFromAbsPos(path.get.reverse(1).xy)
//          }
//        println("\n-----\n")
//        for (node <- path.get) println (node.xy)
//        print("\n ------ \n" + direction)
        "Status(text=missile)|Move(direction=" + nearestEnemy.get.signum + ")"
      }
    }  else {
      "Status(text=missile)"
    }
  }

  def mineReact(params: Map[String, String]) = { //TODO: extract common code for minibots
    val vision = new Vision(params("view"))
    val offsetToMaster: Option[XY] = None // vision.offsetToNearest('m')
    val offsetToSlave = vision.offsetToNearest('b')

    var nearestEnemy: Option[XY] = None

    if (offsetToMaster.isDefined && offsetToSlave.isDefined) {
      nearestEnemy = if (offsetToMaster.get.length > offsetToSlave.get.length) offsetToSlave else offsetToMaster
    } else if (offsetToMaster.isDefined) {
      nearestEnemy = offsetToMaster
    } else if (offsetToSlave.isDefined) {
      nearestEnemy = offsetToSlave
    }


    if (nearestEnemy.isDefined && nearestEnemy.get.length <= 1) {
      "Status(text=mine BOOM)|Explode(size=" + 1 + ")"
    } else {
      "Status(text=mine)"
    }
  }

  def moveSomewhatIntelligently(view: String, params: Map[String, String]) = {
    val vision = new Vision(view)

    var direction: XY = null
    var shoot: XY = null
    var nextStep: String = ""

//    val offsetToNearestMonster = vision.offsetToNearest('b');
//    if (offsetToNearestMonster.isDefined) direction = offsetToNearestMonster.get.signum.negate

//    val offsetToNearestPrey = vision.offsetToNearest('B');
//    if (offsetToNearestPrey.isDefined) direction = offsetToNearestPrey.get.signum
//
//    val offsetToNearestPlant = vision.offsetToNearest('P');
//    if (offsetToNearestPlant.isDefined) direction = offsetToNearestPlant.get.signum

    val offsetToNearestMaster = vision.offsetToNearest('m')
    if (offsetToNearestMaster.isDefined) { shoot = offsetToNearestMaster.get.signum}

    val positionOfNearestPrey = vision.absolutePositionOfNearest('B')
    if (positionOfNearestPrey.isDefined) {
      val path: Option[List[Node]] = AStar(vision.center, positionOfNearestPrey.get, vision)
      if (path.isDefined) {
        val reversePath = path.get.reverse
        direction = vision.relPosFromAbsPos(reversePath(1).xy)
        if (reversePath.size > 2) nextStep = (vision.relPosFromAbsPos(reversePath(2).xy) - direction) + ""
      }
    }

    val positionOfNearestPlant = vision.absolutePositionOfNearest('P')
    if (positionOfNearestPlant.isDefined) {
      val path: Option[List[Node]] = AStar(vision.center, positionOfNearestPlant.get, vision)
      if (path.isDefined) {
        val reversePath = path.get.reverse
        direction = vision.relPosFromAbsPos(reversePath(1).xy)
        if (reversePath.size > 2) nextStep = (vision.relPosFromAbsPos(reversePath(2).xy) - direction) + ""
      }
     // println("going for plant" + direction)
    }

    if (direction == null) {
      val positionOfFarthestSpace = vision.absolutePositionOfFarthest('_')
      if (positionOfFarthestSpace.isDefined) {
        val path: Option[List[Node]] = AStar(vision.center, positionOfFarthestSpace.get, vision)
        if (path.isDefined) direction = vision.relPosFromAbsPos(path.get.reverse(1).xy)
      }
    }

    if (direction == null) {

      val dx = rnd.nextInt(3) - 1
      val dy = rnd.nextInt(3) - 1
      direction = XY(dx, dy)
    }

    if (params.contains("nextstep")) {
      val tokens = params("nextstep").split(':')
      val x = tokens(0).toInt
      val y = tokens(1).toInt
      println("tokens" + x + " " + y)
      direction = XY(x, y)
      nextStep = ""
    }


    val moveCommand =
      if (directionIsSafeToStepOn(direction, vision)) {
        "Move(direction=" + direction + ")"
      } else {
        if (direction.x != 0 && direction.y == 0) direction = direction.updateY(1)
        else if (direction.y != 0 && direction.x == 0) direction = direction.updateX(1)
        else direction = direction.negateX;

        if (directionIsSafeToStepOn(direction, vision)) {
          "Move(direction=" + direction + ")"
        } else {
          ""
        }
      }

  //  println(moveCommand)

    var command: String = null

    if (shoot != null && params("energy").toInt >= 300 && rnd.nextDouble() < 0.45) {

      command = moveCommand + (if (moveCommand != "") "|" else "") + "Spawn(direction=" + shoot + ",heading=" + shoot + ",role=missile,energy=200)"
    } else if (rnd.nextDouble() < 0.05 && params("energy").toInt >= 600 && !vision.offsetToNearest('S').isDefined) {
      command = moveCommand + (if (moveCommand != "") "|" else "") + "Spawn(direction=" + direction.negate + ",role=missile,energy=100)"
    } else {
      command = moveCommand
    }

    command + (if (command != "") "|" else "") + "Set(nextstep=" + nextStep + ")"
  }

  def directionIsSafeToStepOn(direction: XY, vision: Vision) = {val cell = vision.cellAtRelPos(direction); cell != 'W' && cell != 'p'}

}
