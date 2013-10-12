package com.beardedlogic.usecase.lib.change

import com.beardedlogic.usecase.lib.UcParsingCtx

trait ChangeResponder[+V] {
  def respondToChange(c: Change)(implicit ctx: UcParsingCtx): ChangeResult[V, Change]
}
