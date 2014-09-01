package dbaumann.abstractcc

import org.specs2.mutable.Specification
import AbstractCaseClass._

trait AbstractMembers {
  sealed trait AbstractMember

  @`abstract` case class ClassMember(rank: Int, profile: String) extends AbstractMember
  @`abstract` case object ObjectMember extends AbstractMember
}

object ConcreteMembers {
  sealed trait ConcreteMember

  @concrete case class ClassMember(rank: Int, profile: String) extends ConcreteMember

  @concrete case class ClassMemberCompanionOverride(rank: Int, profile: String) extends ConcreteMember
  object ClassMemberCompanionOverride extends java.lang.Runnable { def run = {} }
}

class AbstractCaseClassTest extends Specification {
  import reflect.runtime.universe._

  val abstractDeclarations = typeOf[AbstractMembers].declarations

  "@`abstract` annotation on a case class" should {
    
    "replace the case class member with an abstract type member and implicit abstract classtag member" in {
      val expectations: List[Boolean] = abstractDeclarations.collect {
        case t: TypeSymbol if t.name == newTypeName("ClassMember") && t.isAbstractType =>
          t.typeSignature.asInstanceOf[TypeBounds].hi.typeSymbol.name == newTypeName("AbstractMember")

        case m: MethodSymbol if m.name == newTermName("ClassMemberTag") && m.isImplicit =>
          // how to determine if abstract?
          val tpe = m.typeSignature match { case NullaryMethodType(tpe) => tpe }
          tpe.toString == "scala.reflect.ClassTag[AbstractMembers.this.ClassMember]"
      }.toList

      expectations.size must equalTo(2)
      expectations.forall(identity) must beTrue
    }

    "define an abstract companion object member" in {
      val expectations: List[Boolean] = abstractDeclarations.collect {
        case m: TermSymbol if m.name == newTermName("ClassMember") =>
          // how to determine if abstract?
          val tpe = m.typeSignature match { case NullaryMethodType(tpe) => tpe }
          tpe.toString == "dbaumann.abstractcc.AbstractCaseClass.CompanionInterfaces.CaseClassCompanion2[scala.Int,String,AbstractMembers.this.ClassMember]"
      }.toList

      expectations.size must equalTo(1)
      expectations.forall(identity) must beTrue
    }
  }

  "@`abstract` annotation on a case object" should {

    "replace the case object member with an abstract type member" in {
      val expectations: List[Boolean] = abstractDeclarations.collect {
        case m: TermSymbol if m.name == newTermName("ObjectMember") =>
          // how to determine if abstract?
          val tpe = m.typeSignature match { case NullaryMethodType(tpe) => tpe }
          tpe.toString == "AbstractMembers.this.AbstractMember"
      }.toList

      expectations.size must equalTo(1)
      expectations.forall(identity) must beTrue
    }
  }

  val concreteDeclarations = typeOf[ConcreteMembers.type].declarations

  "@concrete annotation on a case class" should {

    "implement the classtag member" in {
      val expectations: List[Boolean] = concreteDeclarations.collect {
        case m: MethodSymbol if m.name == newTermName("ClassMemberTag") =>
          val tpe = m.typeSignature match { case NullaryMethodType(tpe) => tpe }
          tpe.toString == "scala.reflect.ClassTag[dbaumann.abstractcc.ConcreteMembers.ClassMember]"
      }.toList

      expectations.size must equalTo(1)
      expectations.forall(identity) must beTrue
    }

    "declare the companion object as a CaseClassCompanion, whether explicitly defined or not" in {
      val expectations: List[Boolean] = concreteDeclarations.collect {
        case m: ModuleSymbol if m.name == newTermName("ClassMember") =>
          m.typeSignature.baseClasses.exists(s => s.name == newTypeName("CaseClassCompanion2"))

        case m: ModuleSymbol if m.name == newTermName("ClassMemberCompanionOverride") =>
          m.typeSignature.baseClasses.exists(s => s.name == newTypeName("CaseClassCompanion2")) &&
          m.typeSignature.baseClasses.exists(s => s.name == newTypeName("Runnable"))
      }.toList

      expectations.size must equalTo(2)
      expectations.forall(identity) must beTrue
    }
  }

  // @concrete annotation on a case object is not necessary
}
