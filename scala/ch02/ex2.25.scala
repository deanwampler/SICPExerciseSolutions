
import org.scalatest.FunSuite

object headTailSpec extends FunSuite {
  test ("head and tail traversal") {
    List(1, 3, List(5, 7), 9).tail.tail.head match {
      case head :: tail => assert (tail.head === 7)
      case x => error(x.toString)
    }
    assert(List(List(7)).head.head === 7)
    List(1, List(2, List(3, List(4, List(5, List(6, 7)))))).tail.head match {
      case head :: tail => tail.head match {
        case head :: tail => tail.head match {
          case head :: tail => tail.head match {
            case head :: tail => tail.head match {
              case head :: tail => tail.head match {
                case 7 =>
                case x => error(x.toString)
              }
              case x => error(x.toString)
            }
            case x => error(x.toString)
          }
          case x => error(x.toString)
        }
        case x => error(x.toString)
      }
      case x => error(x.toString)
    }
  }
}
headTailSpec execute