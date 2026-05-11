package shipreq.webapp.member.project.library

import scala.scalajs.js
import shipreq.webapp.base.util.LruMemo
import shipreq.webapp.member.project.data.Project

object CacheJsTestAccess {

  def nonEmpty(empty         : Project,
               latest        : Project,
               milestoneEvery: Int,
               milestones    : js.Array[Project],
               lru           : LruMemo.ExternalFn[Int, Project]): Cache =
    new CacheJs.NonEmpty(empty, latest, milestoneEvery, milestones, lru)
}
