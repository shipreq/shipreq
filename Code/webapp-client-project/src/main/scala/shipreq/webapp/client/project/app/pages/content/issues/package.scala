package shipreq.webapp.client.project.app.pages.content

import japgolly.scalajs.react._

package object issues {

  val RenderFeature = shipreq.webapp.client.project.feature.RenderFeature.ToVdom.NoCtx.IfApplicable

  @inline def shouldComponentUpdate[P: Reusability, C <: Children, S: Reusability, B, U <: UpdateSnapshot]: ScalaComponent.Config[P, C, S, B, U, U] =
    shipreq.webapp.client.project.app.shouldComponentUpdate[P, C, S, B, U]
    // japgolly.scalajs.react.extra.ReusabilityOverlay.install[P, C, S, B, U]

}
