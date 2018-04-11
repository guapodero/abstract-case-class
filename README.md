A Scala macro for converting a case class into its abstract equivalent.

# The problem

Sealed case class hierarchies are a popular alternative to enumerations in Scala, particularly for defining message protocols in actor systems.

__LowLevelProtocol.scala__
```scala
object LowLevelProtocol {
  sealed trait Message

  case class Success(description: String) extends Message
  case class Failure(description: String) extends Message

  case class OtherMessage(other: KitchenSink) extends Message
}
```

In cases where a large protocol must be contained in a single case class heirarchy (e.g. for serialization), decomposition becomes an issue.

__HighLevelImpl.scala__
```scala
import LowLevelProtocol._ // here's the problem

trait ComposableActor extends Actor {
  protected lazy val receiveBuilder = new PartialFunctionBuilder[Any, Unit]
  final def receive = receiveBuilder.result()
}
 
trait BaseService extends ComposableActor {
  receiveBuilder += {
    case Success(description) => ???
    case Failure(description) => ???
  }
}
```

__LowLevelImpl.scala__
```scala
class LowLevelService extends BaseService {
  receiveBuilder += {
    case OtherMessage(other) => ???
  }
}
```

# A solution

Define the high level components of the protocol with abstract members representing the compiler-generated details of the case class.

__HighLevelImpl.scala__
```scala
trait BaseServiceProtocol {
  sealed trait BaseServiceMessage

  type Success <: BaseServiceMessage
  implicit def successTag: reflect.ClassTag[Success]
  val Success: CaseClassCompanion1[String, Success]

  type Failure <: BaseServiceMessage
  implicit def failureTag: reflect.ClassTag[Failure]
  val Failure: CaseClassCompanion1[String, Failure]
}

trait CaseClassCompanion1[T1, R] {
  def apply(v1: T1): R
  def unapply(r: R): Option[T1]
}

trait BaseService extends ComposableActor {
  val baseServiceProtocol: BaseServiceProtocol
  import baseServiceProtocol._ // problem solved

  receiveBuilder += {
    case Success(description) => ???
    case Failure(description) => ???
  }
}
```

Then low level protocols can implement it.

__LowLevelImpl.scala__
```scala
object LowLevelProtocol extends BaseServiceProtocol {
  sealed trait Message

  case class Success(description: String) extends BaseServiceMessage with Message
  def successTag = reflect.classTag[Success]
  object Success extends CaseClassCompanion1[String, Success]

  case class Failure(description: String) extends BaseServiceMessage with Message
  def failureTag = reflect.classTag[Failure]
  object Failure extends CaseClassCompanion1[String, Failure]

  case class OtherMessage(other: KitchenSink) extends Message
}

class LowLevelService extends BaseService {
  val baseServiceProtocol = LowLevelProtocol

  receiveBuilder += {
    case OtherMessage(other) => ???
  }
}
```

Now we have correct dependency inversion, but at the cost of code bloat. Enter Scala macros.

__HighLevelImpl.scala__
```scala
trait BaseServiceProtocol {
  sealed trait BaseServiceMessage

  @`abstract` case class Success(description: String) extends BaseServiceMessage
  @`abstract` case class Failure(description: String) extends BaseServiceMessage
}
...
```

__LowLevelImpl.scala__
```scala
object LowLevelProtocol extends BaseServiceProtocol {
  sealed trait Message

  @concrete case class Success(description: String) extends BaseServiceMessage with Message
  @concrete case class Failure(description: String) extends BaseServiceMessage with Message

  case class OtherMessage(other: KitchenSink) extends Message
}
...
```

# Installation

Add the following dependency to your project:

```scala
libraryDependencies += "org.dbaumann" %% "abstract-case-class" % "0.1"
```

This library is compatible with both Scala 2.10 and 2.11.

# Tests

The tests make use of scala runtime reflection, and only run consistently under Scala 2.11. 2.10 is also supported through the use of deprecated features in scala.reflect.api; if you're using 2.10, you'll just have to take my word that it works.

Also note that compiling under Scala 2.11 comsumes more PermGen space. A workaround is `export SBT_OPTS=-XX:MaxPermSize=256m`.

# Known limitations
 * no support for polymorphic types
 * no support for case class instance methods, only companion objects
 * when abstract case class extends multiple types, the "top" type is assumed to be the first declared
