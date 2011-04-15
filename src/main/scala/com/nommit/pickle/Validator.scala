package com.nommit.pickle

import com.nommit.salt.implicits._
import scalaz._
import Scalaz._

trait Validator[E] {
  def ~(v: DocValidator[E]): DocValidator[E]
  def ~(v: SectionValidator[E]): DocValidator[E]
}

trait DocValidator[E] extends Validator[E] {
  def sectionValidators: Seq[SectionValidator[E]]
  def validate(doc: Doc[Section]): Validation[E, Doc[Section]]
}

trait SectionValidator[E] extends Validator[E] {
  def validate(s: Section): Validation[E, Section]
}

/**
 * A metadata validator is used to validate metadata for an enclosing section validator.
 */
trait MetadataValidator[E] extends Validator[E] {
  type MV = MetadataValidation[E] with DocValidator[E]

  def validate(m: Metadata): Validation[E, Metadata]

  def ~(v: MV): MV
  override def ~(v: DocValidator[E]): MV
  override def ~(v: SectionValidator[E]): MV
}

trait MetadataValidation[E] {
  def metadataValidator: MetadataValidator[E]
}

object Validators {
  object Strict extends Strict[NonEmptyList[String]] {
    override def fail[X](s: String) = s.wrapNel.fail[X]
  }

  abstract class Strict[E](implicit fctor: Functor[({type λ[α] = Validation[E,α]})#λ], app: Apply[({type λ[α] = Validation[E,α]})#λ]) {
    def fail[X](s: String): Validation[E, X]

    trait StrictDocValidator extends DocValidator[E] { parent =>
      def ~(v: DocValidator[E]): DocValidator[E] = new StrictDocValidator {
        def sectionValidators = parent.sectionValidators ++ v.sectionValidators
        def validate(doc: Doc[Section]) = doc.splitAt(parent.sectionValidators.length).mapElements(parent.validate, v.validate).fold { _ <+> _ }
      }

      def ~(v: SectionValidator[E]): DocValidator[E] = new StrictDocValidator {
        def sectionValidators = parent.sectionValidators :+ v
        def validate(doc: Doc[Section]) = doc.splitAt(parent.sectionValidators.length).mapElements(
          parent.validate,
          _.headOption.cata(v.validate, fail("Could not validate section; expected " + parent.sectionValidators.length + " sections but found only " + doc.length))
        ) fold {
          (v1, v2) => ((v1 <**> v2) { (_, _) => doc })
        }
      }
    }

    trait StrictSectionValidator extends SectionValidator[E] { parent =>
      def ~(v: DocValidator[E]): DocValidator[E] = new StrictDocValidator {
        def sectionValidators = parent +: v.sectionValidators
        def validate(doc: Doc[Section]) = {
          val hv = doc.headOption.cata(parent.validate, fail("Could not validate section; document is empty."))
          (hv <**> v.validate(doc.tail)) { (_, _) => doc }
        }
      }

      override def ~(v: SectionValidator[E]): DocValidator[E] = new StrictDocValidator {
        def sectionValidators = Vector(parent, v)
        def validate(doc: Doc[Section]) = {
          val hv  = doc.headOption.cata(parent.validate, fail("Could not validate section; document is emtpy."))
          val thv = doc.tail.headOption.cata(v.validate, fail("Could not validate section; document tail is emtpy."))
          (hv <**> thv) { (_, _) => doc }
        }
      }
    }

    trait StrictMetadataValidator extends MetadataValidator[E] { parent =>
      def ~(v: MV): StrictMetadataDocValidator = new StrictMetadataDocValidator {
        val metadataValidator = if (parent == v.metadataValidator) parent else new StrictMetadataValidator {
          def validate(m: Metadata) = parent.validate(m) <+> v.metadataValidator.validate(m)
        }

        val sectionValidators = v.sectionValidators
        def validate(doc: Doc[Section]) = v.validate(doc)
      }

      def ~(v: DocValidator[E]): StrictMetadataDocValidator = new StrictMetadataDocValidator {
        val metadataValidator = parent
        def sectionValidators = v.sectionValidators
        def validate(doc: Doc[Section])  = v.validate(doc)
      }

      def ~(v: SectionValidator[E]): StrictMetadataDocValidator = new StrictMetadataDocValidator {
        val metadataValidator = parent
        def sectionValidators = Vector(v)
        def validate(doc: Doc[Section]) = doc.headOption.cata(v.validate, fail("Could not validate section; document is emtpy.")).map(_ => doc)
      }
    }

    trait StrictMetadataDocValidator extends MetadataValidation[E] with StrictDocValidator

    val empty: StrictDocValidator = new StrictDocValidator {
      def length = 0
      def sectionValidators = Vector.empty
      def validate(doc: Doc[Section]) = if (doc.isEmpty) doc.success else fail("Doc contains unexpected elements: " + doc)
    }

    implicit def liftSV(sv: SectionValidator[E]) = empty ~ sv

    def ident(sym: Symbol)(v: => MetadataValidation[E] with DocValidator[E]): StrictSectionValidator = new StrictSectionValidator {
      def validate(section: Section) = section match {
        case Complex(Tag(id, metadata), doc) =>
          if (id == sym.name) (v.metadataValidator.validate(metadata) <**> v.validate(doc)) { (m, d) => section }
          else fail("Tag ident mismatch: got " + id + " but expected " + sym.name)

        case s => fail("Bad section: expected tagged complex section but got " + s)
      }
    }

    def metadata(mv: => DocValidator[E]): StrictMetadataValidator = new StrictMetadataValidator {
      def length = 0
      def validate(m: Metadata) = mv.validate(m).map(_ => m)
      def validate(d: Doc[Section]) = empty.validate(d)
    }

    def noMetadata = metadata(empty)

    def text: StrictSectionValidator = new StrictSectionValidator {
      def validate(section: Section) = section match {
        case Complex(_, _) | Separator => fail("Expected primitive text section; got " + section)
        case Primitive(_) => section.success
      }
    }

    def text(s: String): StrictSectionValidator = new StrictSectionValidator {
      def validate(section: Section) = section match {
        case Complex(_, _) | Separator => fail("Expected primitive text section with value \"" + s + "\"; got " + section)
        case Primitive(v) => if (v == s) section.success else fail("Expected primitive text section with value \"" + s + "\"; got " + v)
      }
    }

    def text(p: String => Boolean): StrictSectionValidator = new StrictSectionValidator {
      override def validate(section: Section) = section match {
        case Complex(_, _) | Separator => fail("Expected primitive text section; got " + section)
        case Primitive(s) => if (p(s)) section.success else fail("Primitive section failed validation predicate.")
      }
    }
  }
}

