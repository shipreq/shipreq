package shipreq.webapp.member.test.project

import shipreq.base.test._
import shipreq.base.util.ScalazExtra.applicativeView
import shipreq.base.util.{NonEmptyArraySeq, Validity}
import shipreq.webapp.member.project.text.Atom
import shipreq.webapp.member.project.text.Atom.AnyAtom
import shipreq.webapp.member.test.project.RandomData.{TextGen => $}

object TextShrink {

  def apply[A <: AnyAtom](initialValue: ArraySeq[A])
                         (validity: ArraySeq[A] => Validity): ArraySeq[A] =
    Shrink(
      initialValue = initialValue)(
      shrinker     = optionalTextShrinker[A],
      size         = _.mkString.length,
      validity     = validity,
      breadthLimit = 100)

  def optionalTextShrinker[A <: AnyAtom]: Shrinker[ArraySeq[A]] =
    _optionalTextShrinker
      .emap($.postProcessAtoms($.TopLevelAtom))
      .asInstanceOf[Shrinker[ArraySeq[A]]]

  private lazy val _atomShrinker: Shrinker[AnyAtom] =
    Shrinker.recursive[AnyAtom] { r =>
      Shrinker.iterator { a =>
        def modText   = a.modTextF(Shrinker.string.shrink(_).map(_.value)).iterator.map(r)
        def modTextNE = a.modTextF(Shrinker.nonEmptyString.shrink(_).map(_.value)).iterator.map(r)
        def skip      = Iterator.empty

        a match {

          case _: Atom.CodeBlock # CodeBlock =>
            modText

          case _: Atom.ContentRef # CodeRef =>
            skip

          case _: Atom.ContentRef # ReqRef =>
            skip

          case _: Atom.ContentRef # UseCaseStepRef =>
            skip

          case h: Atom.Headings # Heading1 =>
            _nonEmptyTextShrinker.shrink(h.title).iterator.map(t => r(h.parent.Heading1(t.value.asInstanceOf[h.parent.HeadingTitle])))

          case h: Atom.Headings # Heading2 =>
            _nonEmptyTextShrinker.shrink(h.title).iterator.map(t => r(h.parent.Heading2(t.value.asInstanceOf[h.parent.HeadingTitle])))

          case h: Atom.Headings # Heading3 =>
            _nonEmptyTextShrinker.shrink(h.title).iterator.map(t => r(h.parent.Heading3(t.value.asInstanceOf[h.parent.HeadingTitle])))

          case h: Atom.Headings # Heading4 =>
            _nonEmptyTextShrinker.shrink(h.title).iterator.map(t => r(h.parent.Heading4(t.value.asInstanceOf[h.parent.HeadingTitle])))

          case h: Atom.Headings # Heading5 =>
            _nonEmptyTextShrinker.shrink(h.title).iterator.map(t => r(h.parent.Heading5(t.value.asInstanceOf[h.parent.HeadingTitle])))

          case h: Atom.Headings # Heading6 =>
            _nonEmptyTextShrinker.shrink(h.title).iterator.map(t => r(h.parent.Heading6(t.value.asInstanceOf[h.parent.HeadingTitle])))

          case _: Atom.Issue # Issue =>
            skip

          case l: Atom.ListMarkup # UnorderedList =>
            _listItemsShrinker.shrink(l.items).iterator.map(t => r(l.unsafeWithItems(t.value)))

          case l: Atom.ListMarkup # OrderedList =>
            _listItemsShrinker.shrink(l.items).iterator.map(t => r(l.unsafeWithItems(t.value)))

          case _: Atom.Literal # Literal =>
            modTextNE

          case _: Atom.NewLine # BlankLine =>
            skip

          case x: Atom.PlainTextMarkup # Bold =>
            _nonEmptyTextShrinker.shrink(x.inner).iterator.map(t => r(x.unsafeWithInner(t.value)))

          case x: Atom.PlainTextMarkup # EmailAddress =>
            _emailAddressShrinker.shrink(x).iterator.map(t => r(t.value))

          case x: Atom.PlainTextMarkup # Italic =>
            _nonEmptyTextShrinker.shrink(x.inner).iterator.map(t => r(x.unsafeWithInner(t.value)))

          case _: Atom.PlainTextMarkup # Monospace =>
            modTextNE

          case x: Atom.PlainTextMarkup # Strikethrough =>
            _nonEmptyTextShrinker.shrink(x.inner).iterator.map(t => r(x.unsafeWithInner(t.value)))

          case _: Atom.PlainTextMarkup # TeX =>
            modTextNE

          case x: Atom.PlainTextMarkup # Underline =>
            _nonEmptyTextShrinker.shrink(x.inner).iterator.map(t => r(x.unsafeWithInner(t.value)))

          case x: Atom.PlainTextMarkup # WebAddress =>
            _webAddressShrinker.shrink(x).iterator.map(t => r(t.value))

          case _: Atom.TagRef # TagRef =>
            skip
        }
      }
    }

  private lazy val _optionalTextShrinker: Shrinker[ArraySeq[AnyAtom]] =
    Shrinker.combine[ArraySeq[AnyAtom]](
      Shrinker.removeElements,
      Shrinker.shrinkElements(_atomShrinker)
    )

  private lazy val _nonEmptyTextShrinker: Shrinker[NonEmptyArraySeq[AnyAtom]] =
    Shrinker.combine[NonEmptyArraySeq[AnyAtom]](
      Shrinker.neasRemoveElements,
      Shrinker.neasShrinkElements(_atomShrinker)
    )

  private lazy val _listItemsShrinker: Shrinker[NonEmptyArraySeq[ArraySeq[AnyAtom]]] =
    Shrinker.combine[NonEmptyArraySeq[ArraySeq[AnyAtom]]](
      Shrinker.neasRemoveElements,
      Shrinker.neasShrinkElements(_optionalTextShrinker)
    )

  private lazy val _emailAddressShrinker: Shrinker[Atom.PlainTextMarkup#EmailAddress] =
    Shrinker.constValues("x@x.com").zoom[Atom.PlainTextMarkup#EmailAddress](_.value)(_.copy(_))

  private lazy val _webAddressShrinker: Shrinker[Atom.PlainTextMarkup#WebAddress] =
    Shrinker.constValues("http://x.com").zoom[Atom.PlainTextMarkup#WebAddress](_.value)(_.copy(_))

}
