package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react.Key
import scalaz.{-\/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.data.savedview._
import shipreq.webapp.base.data.savedview.Column._
import shipreq.webapp.base.data
import shipreq.webapp.base.lib.ReactKeyGen
import shipreq.webapp.client.project.feature.{CreateFeature, EditorFeature}

object ColumnExt {

  /** A value that can be passed to React to quickly identify columns. */
  val key: Column => Key = {

    val builtInKeys: BuiltIn => Key =
      builtInValues.iterator
        .map(c => (c, ReactKeyGen.global.next()))
        .foldLeft(Map.empty[BuiltIn, Key])(_ + _)

    {
      case b: BuiltIn     => builtInKeys(b)
      case OtherTags      => "ot"
      case AllTags        => "at"
      case c: CustomField => "f" + c.id.value
    }
  }

  val editorFieldCG = Intersection[Column, EditorFeature.FieldKey.ForCodeGroup] {
    case Column.Code            => Some(EditorFeature.FieldKey.Code)
    case Column.Title           => Some(EditorFeature.FieldKey.CodeGroupTitle)
    case Column.ReqType
       | Column.AllTags
       | Column.OtherTags
       | Column.DeletionReason
       | Column.Pubid
       | _: Column.CustomField
       | _: Column.Implications => None
  } {
    case EditorFeature.FieldKey.Code           => Some(Column.Code)
    case EditorFeature.FieldKey.CodeGroupTitle => Some(Column.Title)
  }

  val editorFieldGR = Intersection[Column, EditorFeature.FieldKey.ForGenericReq] {
    case Column.ReqType                                          => Some(EditorFeature.FieldKey.ReqType)
    case Column.Code                                             => Some(EditorFeature.FieldKey.Codes)
    case Column.Title                                            => Some(EditorFeature.FieldKey.GenericReqTitle)
    case Column.OtherTags                                        => Some(EditorFeature.FieldKey.OtherTags)
    case Column.AllTags                                          => Some(EditorFeature.FieldKey.AllTags)
    case Column.Implications(dir)                                => Some(EditorFeature.FieldKey.Implications(\/-(dir)))
    case Column.CustomField(id: data.CustomField.Implication.Id) => Some(EditorFeature.FieldKey.Implications(-\/(id)))
    case Column.CustomField(id: data.CustomField.Tag        .Id) => Some(EditorFeature.FieldKey.CustomFieldTags(id))
    case Column.CustomField(id: data.CustomField.Text       .Id) => Some(EditorFeature.FieldKey.CustomTextField(id))
    case Column.Pubid
       | Column.DeletionReason                                   => None
  } {
    case EditorFeature.FieldKey.ReqType                => Some(Column.ReqType)
    case EditorFeature.FieldKey.Codes                  => Some(Column.Code)
    case EditorFeature.FieldKey.GenericReqTitle        => Some(Column.Title)
    case EditorFeature.FieldKey.AllTags                => Some(Column.AllTags)
    case EditorFeature.FieldKey.OtherTags              => Some(Column.OtherTags)
    case EditorFeature.FieldKey.Implications(\/-(dir)) => Some(Column.Implications(dir))
    case EditorFeature.FieldKey.Implications(-\/(id))  => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.CustomFieldTags(id)    => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.CustomTextField(id)    => Some(Column.CustomField(id))
  }

  val editorFieldUC = Intersection[Column, EditorFeature.FieldKey.ForUseCase] {
    case Column.Code                                             => Some(EditorFeature.FieldKey.Codes)
    case Column.Title                                            => Some(EditorFeature.FieldKey.UseCaseTitle)
    case Column.AllTags                                          => Some(EditorFeature.FieldKey.AllTags)
    case Column.OtherTags                                        => Some(EditorFeature.FieldKey.OtherTags)
    case Column.Implications(dir)                                => Some(EditorFeature.FieldKey.Implications(\/-(dir)))
    case Column.CustomField(id: data.CustomField.Implication.Id) => Some(EditorFeature.FieldKey.Implications(-\/(id)))
    case Column.CustomField(id: data.CustomField.Tag        .Id) => Some(EditorFeature.FieldKey.CustomFieldTags(id))
    case Column.CustomField(id: data.CustomField.Text       .Id) => Some(EditorFeature.FieldKey.CustomTextField(id))
    case Column.Pubid
       | Column.DeletionReason
       | Column.ReqType                                          => None
  } {
    case EditorFeature.FieldKey.Codes                  => Some(Column.Code)
    case EditorFeature.FieldKey.UseCaseTitle           => Some(Column.Title)
    case EditorFeature.FieldKey.AllTags                => Some(Column.AllTags)
    case EditorFeature.FieldKey.OtherTags              => Some(Column.OtherTags)
    case EditorFeature.FieldKey.Implications(\/-(dir)) => Some(Column.Implications(dir))
    case EditorFeature.FieldKey.Implications(-\/(id))  => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.CustomFieldTags(id)    => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.CustomTextField(id)    => Some(Column.CustomField(id))
  }

  val creationFieldCG: Intersection[Column, CreateFeature.FieldKey.ForCodeGroup] =
    editorFieldCG <=> CreateFeature.FieldKey.editorFieldCG

  val creationFieldGR: Intersection[Column, CreateFeature.FieldKey.ForGenericReq] =
    editorFieldGR <=> CreateFeature.FieldKey.editorFieldGR

  val creationFieldUC: Intersection[Column, CreateFeature.FieldKey.ForUseCase] =
    editorFieldUC <=> CreateFeature.FieldKey.editorFieldUC
}
