import scala.util.Random

class BinaryBehemoth {
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

  def welcome(params: Map[String,String]) = print("got welcome")

  def goodbye(params: Map[String,String]) = print("got goodbye")

  def react(params: Map[String, String]) = params("generation").toInt match {
    case 0 => masterReact(params)
    case 1 => missileReact(params)
  }

  def masterReact(params: Map[String, String]) = {
    moveSomewhatIntelligently(params("view"), params)
  }

  def missileReact(params: Map[String, String]) = {
    val vision = new Vision(params("view"))
    val offsetToMaster = vision.offsetToNearest('m')

    if (offsetToMaster.isDefined) {
      "Status(text=missile)|Move(direction=" + offsetToMaster.get.signum + ")"
    }  else {
      "Status(text=missile)|Move(direction=" + params("heading") + ")"
    }
  }

  def moveSomewhatIntelligently(view: String, params: Map[String, String]) = {
    val vision = new Vision(view)

    var direction: XY = null
    var shoot: XY = null

    val offsetToNearestMonster = vision.offsetToNearest('b');
    if (offsetToNearestMonster.isDefined) direction = offsetToNearestMonster.get.signum.negate

    val offsetToNearestPrey = vision.offsetToNearest('B');
    if (offsetToNearestPrey.isDefined) direction = offsetToNearestPrey.get.signum

    val offsetToNearestPlant = vision.offsetToNearest('P');
    if (offsetToNearestPlant.isDefined) direction = offsetToNearestPlant.get.signum

    val offsetToNearestMaster = vision.offsetToNearest('m')
    if (offsetToNearestMaster.isDefined) { shoot = offsetToNearestMaster.get.signum; direction = offsetToNearestMaster.get.signum.negate}

    if (direction == null) {
      val dx = rnd.nextInt(3) - 1
      val dy = rnd.nextInt(3) - 1
      direction = XY(dx, dy)
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

    if (shoot != null && params("energy").toInt >= 100 && rnd.nextDouble() < 0.05) {
      moveCommand + "|Spawn(direction=" + shoot + ",heading=" + shoot + ")"
    } else {
      moveCommand
    }
  }

  def directionIsSafeToStepOn(direction: XY, vision: Vision) = {val cell = vision.cellAtRelPos(direction); cell != 'W' && cell != 'p'}

}
