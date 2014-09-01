package dbaumann.abstractcc

import scala.reflect.macros._
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

import CrossVersionDefs._

object AbstractCaseClass {
  /**
   * "@`abstract`" macro annotation for case classes
   *
   * This macro annotation converts a case class or case object definition into abstract
   * members which represent the contract of a scala case class companion object.
   * For a case class, a @concrete counterpart is needed to implement the contract.
   */
  class `abstract` extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro abstractMacroImpl
  }

  def abstractMacroImpl(c: CrossVersionContext)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val helper = new Helper[c.type](c)

    def abstractType(upperBound: Option[Tree], className: TypeName) = {
      val bounds = upperBound map { upper =>
        TypeBoundsTree(Ident(c.mirror.staticClass("scala.Nothing")), upper)
      } getOrElse {
        EmptyTree
      }

      TypeDef(Modifiers(Flag.DEFERRED), className, Nil, bounds)
    }

    def abstractTag(className: TypeName) = {
      val tagName = newTermName(className + "Tag")
      q"implicit def $tagName: scala.reflect.ClassTag[$className]"
    }

    def abstractCompanion(baseOpt: Option[Tree], termName: TermName) = {
      val base = baseOpt getOrElse tq"scala.AnyRef"        
      q"val $termName: $base"
    }

    def modifiedDeclaration(decl: Either[ClassDef, ModuleDef]) = decl.fold(
      // case class
      { classDecl =>
        val (className, fields, bases) = helper.extractClassComponents(classDecl)
        val base = helper.companionBase(className, fields)
        
        val typeDecl = abstractType(bases.headOption, className)
        val tagDecl = abstractTag(className)
        val compDecl = abstractCompanion(Some(base), className.toTermName)

        // return abstract members in place of the class definition
        c.Expr(q"""
          $typeDecl
          $tagDecl
          $compDecl
        """)
      },
      // case object
      { objDecl => 
        val (objectName, bases) = helper.extractObjectComponents(objDecl)
        val compDecl = abstractCompanion(bases.headOption, objectName)

        // return abstract member in place of the object definition
        c.Expr(q"""
          $compDecl
        """)
      }
    )

    annottees.map(_.tree) match {
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(Left(classDecl))
      case (compDecl: ModuleDef) :: Nil => modifiedDeclaration(Right(compDecl))
      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }
  }

  /**
   * "@concrete" macro annotation for case classes
   *
   * This macro annotation adds additional definitions to make the annotated class a
   * suitable implementation of an @`abstract` case class. This annotation is not
   * necessary for case objects.
   */
  class concrete extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro concreteMacroImpl
  }

  def concreteMacroImpl(c: CrossVersionContext)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val helper = new Helper[c.type](c)

    def concreteTag(className: TypeName) = {
      val tagName = newTermName(className + "Tag")
      q"def $tagName = scala.reflect.classTag[$className]"
    }

    def concreteCompanion(compDeclOpt: Option[ModuleDef], base: Tree, className: TypeName) = {
      compDeclOpt map { compDecl =>
        val q"object $obj extends ..$bases { ..$body }" = compDecl
        val newBases = bases :+ base
        q"object $obj extends ..$newBases { ..$body }"
      } getOrElse {
        q"object ${className.toTermName} extends $base"
      }
    }

    def modifiedDeclaration(classDecl: ClassDef, compDeclOpt: Option[ModuleDef] = None) = {
      val (className, fields, _) = helper.extractClassComponents(classDecl)
      val base = helper.companionBase(className, fields)

      val tagDecl = concreteTag(className)
      val compDecl = concreteCompanion(compDeclOpt, base, className)

      c.Expr(q"""
        $classDecl
        $tagDecl
        $compDecl
      """)
    }

    annottees.map(_.tree) match {
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)
      case (classDecl: ClassDef) :: (compDecl: ModuleDef) :: Nil => modifiedDeclaration(classDecl, Some(compDecl))
      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }
  }

  private[this] class Helper[C <: CrossVersionContext](val c: C) {
    import c.universe._

    def extractClassComponents(classDecl: ClassDef) = {
      try {
        val q"case class $className(..$fields) extends ..$bases { ..$body }" = classDecl
        (className, fields, bases)
      } catch {
        case _: MatchError => c.abort(c.enclosingPosition, "Annotation is only supported on case class or case object")
      }
    }

    def extractObjectComponents(objectDecl: ModuleDef) = {
      try {
        val q"case object $objectName extends ..$bases { ..$body }" = objectDecl
        (objectName, bases)
      } catch {
        case _: MatchError => c.abort(c.enclosingPosition, "Annotation is only supported on case class or case object")
      }
    }

    def companionBase(className: TypeName, fields: List[ValDef]): Tree = {
      fields.length match {
        case 0 => c.abort(c.enclosingPosition, "Cannot create abstraction for case class with no fields")
        case n => {
          val name = c.mirror.staticClass("dbaumann.abstractcc.AbstractCaseClass.CompanionInterfaces.CaseClassCompanion" + n)
          val tpts: List[Tree] = fields.map(_.tpt)
          tq"""$name[..$tpts, $className]"""
        }
      }
    }
  }

  /**
   * Contracts for case class companion objects, up to arity 22.
   */
  object CompanionInterfaces {
    trait CaseClassCompanion1[T1, R] {
      def apply(v1: T1): R
      def unapply(r: R): Option[T1]
    }

    trait CaseClassCompanion2[T1, T2, R] {
      def apply(v1: T1, v2: T2): R
      def unapply(r: R): Option[(T1, T2)]
    }

    trait CaseClassCompanion3[T1, T2, T3, R] {
      def apply(v1: T1, v2: T2, v3: T3): R
      def unapply(r: R): Option[(T1, T2, T3)]
    }

    trait CaseClassCompanion4[T1, T2, T3, T4, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4): R
      def unapply(r: R): Option[(T1, T2, T3, T4)]
    }

    trait CaseClassCompanion5[T1, T2, T3, T4, T5, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5)]
    }

    trait CaseClassCompanion6[T1, T2, T3, T4, T5, T6, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6)]
    }

    trait CaseClassCompanion7[T1, T2, T3, T4, T5, T6, T7, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7)]
    }

    trait CaseClassCompanion8[T1, T2, T3, T4, T5, T6, T7, T8, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8)]
    }

    trait CaseClassCompanion9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9)]
    }

    trait CaseClassCompanion10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)]
    }

    trait CaseClassCompanion11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)]
    }

    trait CaseClassCompanion12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)]
    }

    trait CaseClassCompanion13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)]
    }

    trait CaseClassCompanion14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)]
    }

    trait CaseClassCompanion15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)]
    }

    trait CaseClassCompanion16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)]
    }

    trait CaseClassCompanion17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)]
    }

    trait CaseClassCompanion18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)]
    }

    trait CaseClassCompanion19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)]
    }

    trait CaseClassCompanion20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)]
    }

    trait CaseClassCompanion21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)]
    }

    trait CaseClassCompanion22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] {
      def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22): R
      def unapply(r: R): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)]
    }
  }
}
