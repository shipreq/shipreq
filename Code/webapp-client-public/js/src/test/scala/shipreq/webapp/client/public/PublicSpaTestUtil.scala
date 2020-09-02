package shipreq.webapp.client.public

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.base.util._
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.base.protocol.ajax.TestAjaxClient
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.public.spa.{Page, PublicSpa}
import teststate.data.Id

object PublicSpaTestUtil {

  val initData = PublicSpaEntryPoint.InitData(Allow, None, AssetManifest(None))

  class ForTestState {
    val ajax     = new TestAjaxClient(false)
    val rc       = MockRouterCtl[Page]()
    var initData = PublicSpaTestUtil.initData

    def render[A](initPage: Page)(f: DomZipperJs => A): A = {
      val spa = new PublicSpa(initData, ajax)
      ReactTestUtils.withRenderedIntoDocument(spa.Component(PublicSpa.Props(initPage, rc, initData.assetManifest))) { m =>
        f(m.domZipper)
      }
    }

    def apply(initPage: Page)(f: DomZipperJs => Report[String]): Unit =
      render(initPage) { h =>
        val r = f(h)
        assertTestState(r)
        ()
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  abstract class InputFieldObs($: DomZipperJs, val input: html.Input) {
    val failure : Option[String] = $.collect01(">div:not(.ui),>span").innerTexts
    val enabled : Enabled        = Disabled.when(input.disabled || semanticUiDisabled($.domAsHtml))
    val validity: Validity       = Invalid when $.domAsHtml.classList.contains("error")
  }

  class TextFieldObs($: DomZipperJs) extends InputFieldObs($,
    $("input[type=text],input[type=email],input[type=password],textarea").domAs[html.Input]) {
    val value: String = input.value
  }

  class CheckboxFieldObs($: DomZipperJs) extends InputFieldObs($,
    $("input[type=checkbox]").domAs[html.Input]) {
    val checked: Boolean = input.checked
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  class InputFieldDsl[R, O, S](final val * : Dsl[Id, R, O, S, String], desc: String, f: O => InputFieldObs) {
    val enabled    = *.focus(desc).value($ => f($.obs).enabled)
    val validity   = *.focus(desc).value($ => f($.obs).validity)
    val failure    = *.focus(s"$desc failure").value($ => f($.obs).failure)
    val hasFailure = failure.map(_.isDefined)
  }

  class TextFieldDsl[R, O, S](dsl: Dsl[Id, R, O, S, String])(desc: String, f: O => TextFieldObs) extends InputFieldDsl(dsl, desc, f) {
    val text = *.focus(desc).value($ => f($.obs).value)
    val tv   = *.focus(s"$desc text & validity").value($ => (f($.obs).value, f($.obs).validity))

    def set(text: String): *.Actions =
      *.action(s"Set $desc to [$text]")($ => SimEvent.Change(text) simulate f($.obs).input) <+ enabled.assert(Enabled)
  }

  class CheckboxFieldDsl[R, O, S](dsl: Dsl[Id, R, O, S, String])(desc: String, f: O => CheckboxFieldObs) extends InputFieldDsl(dsl, desc, f) {
    val checked = *.focus(s"$desc.checked").value($ => f($.obs).checked)

    lazy val click: *.Actions =
      *.action(s"Click $desc")($ => Simulate change f($.obs).input) <+ enabled.assert(Enabled)

    lazy val check: *.Actions =
      click.unless($ => f($.obs).checked).rename(s"Check $desc")

    lazy val uncheck: *.Actions =
      click.when($ => f($.obs).checked).rename(s"Uncheck $desc")
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  @tailrec
  def semanticUiDisabled(h: html.Element): Boolean =
    if (h.classList.contains("disabled"))
      true
    else {
      val n = h.parentElement
      if (n eq null)
        false
      else
        semanticUiDisabled(n)
    }
}
