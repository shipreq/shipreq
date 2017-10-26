package shipreq.webapp.client.project.app.reqtable

import scalaz.{-\/, \/-}
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.data.reqtable.Column._
import shipreq.webapp.base.data
import shipreq.webapp.base.lib.KeyGen
import shipreq.webapp.client.project.feature.{CreateFeature, EditorFeature}

object ColumnExt {

  /** A value that can be passed to React to quickly identify columns. */
  val key: Column => String = {

    val builtInKeys: BuiltIn => String =
      builtInValues.iterator
        .map(c => (c, KeyGen.global.next()))
        .foldLeft(UnivEq.emptyMap[BuiltIn, String])(_ + _)

    {
      case b: BuiltIn     => builtInKeys(b)
      case c: CustomField => "f" + c.id.value
    }
  }

  val editorFieldCG = Intersection[Column, EditorFeature.FieldKey.ForCodeGroup] {
    case Column.Code            => Some(EditorFeature.FieldKey.Code)
    case Column.Title           => Some(EditorFeature.FieldKey.CodeGroupTitle)
    case Column.ReqType
       | Column.Tags
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
    case Column.Tags                                             => Some(EditorFeature.FieldKey.Tags(None))
    case Column.Implications(dir)                                => Some(EditorFeature.FieldKey.Implications(\/-(dir)))
    case Column.CustomField(id: data.CustomField.Implication.Id) => Some(EditorFeature.FieldKey.Implications(-\/(id)))
    case Column.CustomField(id: data.CustomField.Tag        .Id) => Some(EditorFeature.FieldKey.Tags(Some(id)))
    case Column.CustomField(id: data.CustomField.Text       .Id) => Some(EditorFeature.FieldKey.CustomTextField(id))
    case Column.Pubid
       | Column.DeletionReason                                   => None
  } {
    case EditorFeature.FieldKey.ReqType                => Some(Column.ReqType)
    case EditorFeature.FieldKey.Codes                  => Some(Column.Code)
    case EditorFeature.FieldKey.GenericReqTitle        => Some(Column.Title)
    case EditorFeature.FieldKey.Tags(None)             => Some(Column.Tags)
    case EditorFeature.FieldKey.Implications(\/-(dir)) => Some(Column.Implications(dir))
    case EditorFeature.FieldKey.Implications(-\/(id))  => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.Tags(Some(id))         => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.CustomTextField(id)    => Some(Column.CustomField(id))
  }

  val editorFieldUC = Intersection[Column, EditorFeature.FieldKey.ForUseCase] {
    case Column.Code                                             => Some(EditorFeature.FieldKey.Codes)
    case Column.Title                                            => Some(EditorFeature.FieldKey.UseCaseTitle)
    case Column.Tags                                             => Some(EditorFeature.FieldKey.Tags(None))
    case Column.Implications(dir)                                => Some(EditorFeature.FieldKey.Implications(\/-(dir)))
    case Column.CustomField(id: data.CustomField.Implication.Id) => Some(EditorFeature.FieldKey.Implications(-\/(id)))
    case Column.CustomField(id: data.CustomField.Tag        .Id) => Some(EditorFeature.FieldKey.Tags(Some(id)))
    case Column.CustomField(id: data.CustomField.Text       .Id) => Some(EditorFeature.FieldKey.CustomTextField(id))
    case Column.Pubid
       | Column.DeletionReason
       | Column.ReqType                                          => None
  } {
    case EditorFeature.FieldKey.Codes                  => Some(Column.Code)
    case EditorFeature.FieldKey.UseCaseTitle           => Some(Column.Title)
    case EditorFeature.FieldKey.Tags(None)             => Some(Column.Tags)
    case EditorFeature.FieldKey.Implications(\/-(dir)) => Some(Column.Implications(dir))
    case EditorFeature.FieldKey.Implications(-\/(id))  => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.Tags(Some(id))         => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.CustomTextField(id)    => Some(Column.CustomField(id))
  }

  val creationFieldCG: Intersection[Column, CreateFeature.FieldKey.ForCodeGroup] =
    editorFieldCG <=> CreateFeature.FieldKey.editorFieldCG

  val creationFieldGR: Intersection[Column, CreateFeature.FieldKey.ForGenericReq] =
    editorFieldGR <=> CreateFeature.FieldKey.editorFieldGR

  val creationFieldUC: Intersection[Column, CreateFeature.FieldKey.ForUseCase] =
    editorFieldUC <=> CreateFeature.FieldKey.editorFieldUC
}
