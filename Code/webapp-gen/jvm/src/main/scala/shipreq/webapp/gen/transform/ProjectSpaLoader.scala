package shipreq.webapp.gen.transform

import scala.xml.Utility.escape
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.user.Username
import shipreq.webapp.gen._

object ProjectSpaLoader extends Transformer(
    shipreq.webapp.gen.output.ProjectSpaLoader.templates,
    Data.projectSpaLoaderData) {

   def apply(i: (Username, Project.Name)): Html =
    templates.main.map(_
      .replace(data.main._1.value, i._1.value)
      .replace(data.main._2, escape(i._2)))
}
