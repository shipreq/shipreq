package com.beardedlogic.usecase.feature.uc.change

import scalaz.NonEmptyList
import com.beardedlogic.usecase.feature.uc.UcParsingCtx
import com.beardedlogic.usecase.lib.ScalazSubset._


trait ChangeResponder[V] {
  type R = ChangeResult[V, Change]
  def respondToChanges(value: V, changes: NonEmptyList[Change])(implicit ctx: UcParsingCtx): R
}

trait SeqChangeResponder[V] extends ChangeResponder[V] {

  def respondToChanges(value: V, changes: NonEmptyList[Change])(implicit ctx: UcParsingCtx): R =
    changes.foldLeft(NoChange: R)((result, change) =>
      result.andThen(value, respondToChange(_, change)))

  def respondToChange(value: V, change: Change)(implicit ctx: UcParsingCtx): R
}